{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module KnowledgeBase where

import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Text              as T
import           System.FilePath
import Streaming (chunksOf)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as Str
import qualified System.Directory as System
import Data.Foldable
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Function ((&))
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Control.Concurrent.Async
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Dependency
import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import qualified Data.List as List
import qualified Data.Text.IO as T
import Debug.Trace (traceShowM, traceShow)
import Distribution.PackageDescription.Configuration

-- | Loads and parses a Cabal file
loadFile ::
  (MonadIO m) =>
  -- | The absolute path to the Cabal file
  FilePath ->
  m GenericPackageDescription
loadFile path = liftIO $ readGenericPackageDescription silent path

loadAndExtractCabalFile :: FilePath -> IO (Maybe (PackageName, Version, [Dependency]))
loadAndExtractCabalFile fp = do
  gpk <- readGenericPackageDescription silent fp
  pure $ getBuildConstraints (flattenPackageDescription gpk)

getBuildConstraints :: PackageDescription -> Maybe
     (PackageName,
      Version,
      [Dependency])
getBuildConstraints pd = do
  lib <- library pd
  let bi = libBuildInfo lib
      bd = targetBuildDepends bi
      pkg = package pd
  pure (pkgName pkg, pkgVersion pkg, bd)

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: MonadIO m => FilePath -> m ()
importAllFilesInDirectory dir = do
  parallelWorkers <- liftIO getNumCapabilities
  let chunkSize = 200
  countMVar <- liftIO $ newMVar @Int 0
  findAllCabalFilesInDirectory dir
    & parMapM parallelWorkers (liftIO . loadAndExtractCabalFile)
    & chunksOf chunkSize
    & Str.mapped Str.toList
    & Str.mapM_ (persistChunk countMVar)
  where
    persistChunk :: (MonadIO m, Foldable f) => MVar Int -> f (Maybe (PackageName, Version, [Dependency])) -> m ()
    persistChunk countMvar chunk = do
      let size = length chunk
      newCount <- liftIO $ modifyMVar countMvar (\c -> pure (c + size, c + size))
      forM_ chunk $ \case
        Nothing -> liftIO $ putStrLn "Nothing"
        Just triple@(name, ver, _) -> do
          let x = prettyDeps triple
              fp = T.unpack (prettyName name) <> "-" <> T.unpack (prettyVersion ver) <.> "txt"
          liftIO $ T.writeFile ("big-db" </> fp) x

      liftIO . putStrLn $ "✅ Processed " <> show newCount <> " new cabal files"

prettyDeps :: (PackageName, Version, [Dependency]) -> T.Text
prettyDeps (name, version, deps) =
  T.unlines $
    [ "pkg(" <> prettyName name <> ", (" <> prettyVersion version <> "))."] <>
    [ constraint <> " :- " <> ifChosen <> "."
    | dep <- deps
    , constraint <- constraints dep
    ]
  where
    ifChosen = "chosen(" <> prettyName name <> ", (" <> prettyVersion version <> "))"

prettyName :: PackageName -> T.Text
prettyName = slugifyName . T.pack . unPackageName

prettyVersion :: Version -> T.Text
prettyVersion version =
  let nums = versionNumbers version
      normalised = nums <> List.repeat 0
      pvpNums = take 4 normalised
      pvpString = fmap (T.pack . show) pvpNums
  in
    T.intercalate ", " pvpString

prettyVersionConstraint :: Version -> T.Text
prettyVersionConstraint version =
  let nums = versionNumbers version
      normalised = nums <> List.repeat (-1)
      pvpNums = take 4 normalised
      pvpString = fmap (T.pack . show) pvpNums
  in
    T.intercalate ", " pvpString

constraints :: Dependency -> [T.Text]
constraints dep = prettyVersionRange (depVerRange dep)
  where
    name = prettyName $ depPkgName dep
    prettyVersionRange :: VersionRange -> [T.Text]
    prettyVersionRange = \case
      AnyVersion -> ["wanted(" <> name <> ")"]
      ThisVersion ver -> ["chosen(" <> name <> ", (" <> prettyVersionConstraint ver <> ")"]
      LaterVersion ver -> [] -- TODO:
      OrLaterVersion ver -> ["get(" <> name <> ", (" <> prettyVersionConstraint ver <> ")"]
      EarlierVersion ver -> ["lt(" <> name <> ", (" <> prettyVersionConstraint ver <> ")"]
      OrEarlierVersion ver -> [] -- TODO:
      WildcardVersion ver -> [] -- TODO:
      MajorBoundVersion ver -> prettyVersionRange $ majorBoundVersion ver
      UnionVersionRanges verR1 verR2 -> [] -- TODO:
      IntersectVersionRanges verR1 verR2 ->
        prettyVersionRange verR1 <> prettyVersionRange verR2
      VersionRangeParens verR ->
        prettyVersionRange verR

slugifyName :: T.Text -> T.Text
slugifyName = T.replace "-" "_"

{- | Finds all cabal files in the provided directory recursively
 Hits are written to the output channel as they are found, so it should be possible to process
 large amounts of Cabal files efficiently
-}
findAllCabalFilesInDirectory ::
  MonadIO m =>
  FilePath ->
  Stream (Of FilePath) m ()
findAllCabalFilesInDirectory workdir = do
  liftIO . putStrLn $ "🔎  Searching cabal files in " <> workdir
  liftIO $ System.createDirectoryIfMissing True workdir
  inspectDir workdir
  where
    inspectDir dir = liftIO (System.listDirectory dir) >>= traverse_ (inspectItem dir)
    inspectItem dir item = do
      let fullPath = dir </> item
      isDir <- liftIO $ System.doesDirectoryExist fullPath
      case isDir of
        True -> inspectDir fullPath
        False | ".cabal" `isSuffixOf` fullPath -> Str.yield fullPath
        _ -> pure ()

-- | Replaces each element of a stream with the result of an action, processing elements of the stream concurrently
parMapM :: MonadIO m => Int -> (a -> IO b) -> Stream (Of a) m () -> Stream (Of b) m ()
parMapM concurrentActions f str =
  Str.mapM (liftIO . async . f) str
    & chunksOf concurrentActions
    & Str.mapped Str.toList
    & Str.mapM (liftIO . traverse wait)
    & Str.concat
