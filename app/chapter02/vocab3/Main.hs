{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text.IO                    as TIO
import           HaskellInDepth.Chapter02.Vocab3
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname, n] -> processTextFile fname (read n)
        _ -> TIO.putStrLn "Usage: vocab3 filename number_of_frequent_words"

-- | Processes a text file at a given path and prints out words
processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordsReport vocab n
