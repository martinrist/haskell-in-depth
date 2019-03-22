module HaskellInDepth.Chapter02.Vocab2 (processTextFile) where

import           Data.Char
import           Data.List
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment

-- | A single entry, consisting of a word and its number of occurrences
type Entry = (T.Text, Int)

-- | A list of entries
type Vocabulary = [Entry]

-- | Extracts a `Vocabulary` from a piece of text
extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
    where
        ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
        buildEntry ws@(w:_) = (w, length ws)
        cleanWord = T.dropAround (not . isLetter)

-- | Prints out all the words in a `Vocabulary`
printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
    putStrLn "All words:"
    TIO.putStrLn $ T.unlines $ map fst vocab

-- | Processes a text file at a given path and prints out words
processTextFile :: FilePath -> IO ()
processTextFile fname = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    printAllWords vocab
