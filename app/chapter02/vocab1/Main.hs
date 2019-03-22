module Main where

import           Data.Text                      as T
import           Data.Text.IO                   as TIO
import           HaskellInDepth.Chapter02.Vocab1
import           System.Environment

main :: IO ()
main = do
    [fname] <- getArgs
    text <- TIO.readFile fname
    let (x, y) = getWordAnalysis text
    TIO.putStrLn x
    print y
