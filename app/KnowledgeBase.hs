{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module KnowledgeBase(importAllFilesInDirectory, loadAndExtractCabalFile, prettyDeps) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import Data.IORef (IORef, atomicModifyIORef, atomicModifyIORef', newIORef)
import Data.List
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace (traceM, traceShow)
import Distribution.PackageDescription
    ( BuildInfo(targetBuildDepends),
      Library(libBuildInfo),
      PackageDescription(library, package) )
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Types.Dependency
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionInterval
import Distribution.Types.VersionRange.Internal
import Distribution.Verbosity
import Streaming (chunksOf)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as Str
import qualified System.Directory as System
import System.FilePath

data RuleType
  = SimpleRule T.Text
  | DisjRule T.Text
  | WantedRule T.Text
  deriving (Show, Eq, Ord)


loadAndExtractCabalFile :: FilePath -> IO (Maybe (PackageName, Version, [Dependency]))
loadAndExtractCabalFile fp = do
  gpk <- readGenericPackageDescription silent fp
  -- traceM $ "Parsing " ++ fp
  pure $ getBuildConstraints (flattenPackageDescription gpk)

getBuildConstraints ::
  PackageDescription ->
  Maybe
    ( PackageName
    , Version
    , [Dependency]
    )
getBuildConstraints pd = do
  lib <- library pd
  let bi = libBuildInfo lib
      bd = targetBuildDepends bi
      pkg = package pd
  -- traceM $ "Got constraints of " ++ show (pkgName pkg)
  pure (pkgName pkg, pkgVersion pkg, bd)

-- | Finds all cabal files in the specified directory, and inserts them into the database after extracting the relevant data
importAllFilesInDirectory :: MonadIO m => FilePath -> m ()
importAllFilesInDirectory dir = do
  ref <- liftIO $ newIORef 0
  parallelWorkers <- liftIO getNumCapabilities
  let chunkSize = 200
  countMVar <- liftIO $ newMVar @Int 0
  findAllCabalFilesInDirectory dir
    & parMapM parallelWorkers (liftIO . loadAndExtractCabalFile)
    & chunksOf chunkSize
    & Str.mapped Str.toList
    & Str.mapM_ (persistChunk ref countMVar)
 where
  persistChunk :: (MonadIO m, Foldable f) => IORef Int -> MVar Int -> f (Maybe (PackageName, Version, [Dependency])) -> m ()
  persistChunk ref countMvar chunk = do
    let size = length chunk
    newCount <- liftIO $ modifyMVar countMvar (\c -> pure (c + size, c + size))
    forM_ chunk $ \case
      Nothing -> pure ()
      Just triple@(name, ver, _deps) -> do
        x <- liftIO $ prettyDeps ref triple
        let fp = T.unpack (prettyName' name) <> "-" <> T.unpack (fileNameVersion ver) <.> "txt"
        liftIO $ T.writeFile ("big-db" </> fp) x

    liftIO . putStrLn $ "✅ Processed " <> show newCount <> " new cabal files"

fileNameVersion :: Version -> T.Text
fileNameVersion version =
  let nums = versionNumbers version
      normalised = nums <> List.repeat 0
      pvpNums = take 4 normalised
      pvpString = fmap (T.pack . show) pvpNums
   in slugifyName $ T.intercalate "_" pvpString

prettyDeps :: IORef Int -> (PackageName, Version, [Dependency]) -> IO T.Text
prettyDeps ref (name, version, deps) = do
  myConstraints <- mapM (constraints ref) deps
  pure $
    T.unlines $
      ["pkg(" <> prettyName name <> ", (" <> prettyVersion version <> "))."]
        <> [ prettyRuleType c
           | constraint <- myConstraints
           , c <- constraint
           ]
 where
  ifChosen = "chosen(" <> prettyName name <> ", (" <> prettyVersion version <> "))"
  prettyRuleType :: RuleType -> T.Text
  prettyRuleType (SimpleRule r) = r <> " :- " <> ifChosen <> "."
  prettyRuleType (DisjRule r) = r <> "."
  prettyRuleType (WantedRule r) = r <> " :- " <> ifChosen <> "."

prettyName' :: PackageName -> T.Text
prettyName' = slugifyName . T.pack . unPackageName


prettyName :: PackageName -> T.Text
prettyName n = "\"" <> (slugifyName . T.pack $ unPackageName n) <> "\""

prettyVersion :: Version -> T.Text
prettyVersion version =
  let nums = versionNumbers version
      normalised = nums <> List.repeat 0
      pvpNums = take 4 normalised
      pvpString = fmap (T.pack . show) pvpNums
   in T.intercalate ", " pvpString

prettyVersionConstraint :: Version -> T.Text
prettyVersionConstraint version =
  let nums = versionNumbers version
      normalised = nums <> List.repeat (-1)
      pvpNums = take 4 normalised
      pvpString = fmap (T.pack . show) pvpNums
   in T.intercalate ", " pvpString

versionSubtractOne :: Version -> Version
versionSubtractOne v =
  if last listVer == 0
    then versionSubtractOne $ mkVersion $ init listVer
    else mkVersion $ init listVer ++ [last listVer - 1]
 where
  listVer = versionNumbers v

versionAddOne :: Version -> Version
versionAddOne v = mkVersion $ init listVer' ++ [last listVer' + 1]
 where
  listVer' = versionNumbers v

constraints :: IORef Int -> Dependency -> IO [RuleType]
constraints ref dep = do
  pVR <- prettyVersionRange (depVerRange dep)
  let wantedRule = WantedRule $ "wanted(" <> name <> ")"
  pure $ wantedRule : pVR
 where
  name = prettyName $ depPkgName dep
  prettyVersionRange :: VersionRange -> IO [RuleType]
  prettyVersionRange vr =
    -- traceShow (asVersionIntervals vr) $
    case asVersionIntervals vr of
    [] -> pure [SimpleRule $ "error(\"invalid package version chosen\"," <> name <> ")"]
    [vI] -> pure $ intervalToRule vI
    vIntervals -> do
      ruleNamesVersions <-
        forM
          vIntervals
          ( \v -> do
              rId <- atomicModifyIORef' ref (\x -> (x + 1, x))
              let rName = T.pack $ "genRule" <> show rId
              let prettyVer = intervalToRule v
              pure (rName, prettyVer)
          )
      pure
        ( SimpleRule ("{ " <> T.intercalate " ; " (map fst ruleNamesVersions) <> " }")
            : [ processNestedDisjunction rulename rule
              | rNV <- ruleNamesVersions
              , let rulename = fst rNV
              , rule <- snd rNV
              ]
        )
  intervalToRule :: VersionInterval -> [RuleType]
  intervalToRule (LowerBound lb InclusiveBound, UpperBound ub InclusiveBound) =
    [ SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint lb <> "))"
    , SimpleRule $ "lt(" <> name <> ", (" <> prettyVersionConstraint (versionAddOne ub) <> "))"
    ]
  intervalToRule (LowerBound lb ExclusiveBound, UpperBound ub InclusiveBound) =
    [ SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint (versionAddOne lb) <> "))"
    , SimpleRule $ "lt(" <> name <> ", (" <> prettyVersionConstraint (versionAddOne ub) <> "))"
    ]
  intervalToRule (LowerBound lb InclusiveBound, UpperBound ub ExclusiveBound) =
    [ SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint lb <> "))"
    , SimpleRule $ "lt(" <> name <> ", (" <> prettyVersionConstraint ub <> "))"
    ]
  intervalToRule (LowerBound lb ExclusiveBound, UpperBound ub ExclusiveBound) =
    [ SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint (versionAddOne lb) <> "))"
    , SimpleRule $ "lt(" <> name <> ", (" <> prettyVersionConstraint ub <> "))"
    ]
  intervalToRule (LowerBound lb InclusiveBound, NoUpperBound) =
    [SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint lb <> "))"]
  intervalToRule (LowerBound lb ExclusiveBound, NoUpperBound) =
    [SimpleRule $ "get(" <> name <> ", (" <> prettyVersionConstraint (versionAddOne lb) <> "))"]

  processNestedDisjunction :: T.Text -> RuleType -> RuleType
  processNestedDisjunction n (SimpleRule rT) = DisjRule (rT <> " :- " <> n)
  processNestedDisjunction _ (DisjRule rT) = DisjRule rT
  processNestedDisjunction _ (WantedRule _) = error "disjunction on wanted rule"

slugifyName :: T.Text -> T.Text
slugifyName = T.replace " " "" . T.replace "-" "_"

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
