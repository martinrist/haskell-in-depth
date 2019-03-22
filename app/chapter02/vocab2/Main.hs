module Main where

import           HaskellInDepth.Chapter02.Vocab2
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _       -> putStrLn "Usage: vocab2 filename"
