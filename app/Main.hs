module Main where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           KnowledgeBase
import           Parser
import           System.FilePath
import           Text.Megaparsec (errorBundlePretty)


main :: IO ()
main = do
    input <- T.readFile ("testdata" </> "testdeps.txt")
    case parser input of Left err -> putStrLn $ errorBundlePretty err
                         Right out ->
                             do
                                 T.writeFile ("testdata" </> "testinputs.txt") $ T.unlines $ map constraintRule out
                                 run out
