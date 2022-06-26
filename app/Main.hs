module Main where

import           KnowledgeBase
import System.Directory.Internal.Prelude (getArgs)


main :: IO ()
main = do
    [dir] <- getArgs
    importAllFilesInDirectory dir
