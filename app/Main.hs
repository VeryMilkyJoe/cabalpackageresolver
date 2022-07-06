module Main where

import           KnowledgeBase
import System.Environment (getArgs)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.HashMap.Strict (HashMap)


main :: IO ()
main = do
    [dir] <- getArgs
    importAllFilesInDirectory dir

getKBForCabalFile :: FilePath -> IO ()
getKBForCabalFile fp = do
    mPkgInfo <- loadAndExtractCabalFile fp
    case mPkgInfo of
        Nothing -> error $ "cabal file to extract does not exist at " ++ fp
        (Just pkgInfo) ->
            do
            ref <- newIORef 50000000
            ruleTypes <- prettyDeps ref pkgInfo
            T.writeFile "myCabalRules.txt" ruleTypes
            pure ()
    pure ()

-- map with packagenames as keys and all filepaths corresponding to all existing versions of the package as values
foo :: HashMap T.Text [FilePath] -> [T.Text] -> IO (HashMap T.Text [FilePath])
foo lookupTable = undefined
