module HaskellInDepth.Chapter02.Vocab1 where

import           Data.Char
import           Data.List
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO

getWordAnalysis :: T.Text -> (T.Text, Int)
getWordAnalysis text =
    let ws =
            map head $
            group $
            sort $
            map T.toCaseFold $
            filter (not . T.null) $
            map (T.dropAround $ not . isLetter) $ T.words text
     in (T.unwords ws, length ws)