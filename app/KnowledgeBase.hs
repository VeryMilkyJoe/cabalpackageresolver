{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KnowledgeBase where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Key         as Aeson
import qualified Data.Aeson.KeyMap      as Aeson
import           Data.List
import           Data.Maybe             (mapMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Network.HTTP.Req
import           Parser
import           System.Directory
import           System.FilePath

run :: [Constraint] -> IO ()
run input = do
    let wanted = mapMaybe (\case
            Wanted name -> Just name
            _           -> Nothing) input
        pkgNames = nub $ sort wanted

    print pkgNames

    -- create dir for cached version files for each pkg
    createDirectoryIfMissing True "versions"
    runReq defaultHttpConfig $ forM_ pkgNames $ \pkg -> do
        let pkgS = T.unpack pkg
        -- check if file for pkg is already cached
        exists <- liftIO $ doesFileExist $ "versions" </> pkgS <.> "txt"
        if exists
            then liftIO $ putStrLn $ "Package " ++ pkgS ++ " already cached"
            else do
                liftIO $ putStrLn $ "downloading versions for: " ++ pkgS
                json <- req GET (http "hackage.haskell.org" /: "package" /: pkg) NoReqBody jsonResponse (header "ACCEPT" "application/json")
                case responseBody json of
                    Aeson.Object obj -> do
                        -- keys correspond to versions, we do not treat deprecated packages so the values are ignored
                        let versions = map Aeson.toText $ Aeson.keys obj
                            prettyVersions = buildKnownVersions pkg versions
                        liftIO $ T.writeFile ("versions" </> pkgS <.> "txt") (T.unlines prettyVersions)
                    _ -> liftIO $ putStrLn $ "Unexpected json! Json: " ++ show json

    -- create knowledge base file for resolver
    T.writeFile ("testdata" </> "knowledge.txt") ""
    forM_ pkgNames $ \pkg -> do
        T.readFile ("versions" </> T.unpack pkg <.> "txt") >>= T.appendFile "knowledge.txt"


buildKnownVersions :: Name -> [T.Text] -> [T.Text]
buildKnownVersions name versions =
    map (\v -> "pkg(" <> slugifyName name <> ", (" <> T.intercalate ", " (fullVersion v) <> "))." ) versions
    where
        fullVersion :: T.Text -> [T.Text]
        -- add zeros at the end if there are less than 4 version components
        fullVersion v = take 4 (T.splitOn "." v ++ repeat "0")
