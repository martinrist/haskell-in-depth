{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.Chapter02.Vocab3 where

import           Data.Char
import           Data.List
import           Data.Ord
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

-- | Produces a report containing all the words
allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab = T.append "\nAll words:\n" $ T.unlines $ map fst vocab

-- | Counts the total number of words in the `Vocabulary`
wordsCount :: Vocabulary -> Int
wordsCount vocab = sum $ map snd vocab

-- | Produces a report of the total word count
wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab =
    T.append "\nTotal number of words: " $ T.pack $ show $ wordsCount vocab

-- | Sorts the `Vocabulary` in decreasing order of word frequency
wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

-- | Produces a report of the most frequently-used words
frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n =
    T.append "\nFrequent words:\n" $
    T.unlines $ map showEntry $ take n $ wordsByFrequency vocab
  where
    showEntry (t, n) = T.append t $ T.pack $ " - " ++ show n


