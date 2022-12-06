{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join)
import Data.Array (inRange)
import Data.Char (ord)
import Data.Foldable qualified as F
import Data.List (intersect, sort)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T

intersection :: [String] -> String
intersection = S.toList . S.fromList . foldr1 intersect

splitHalf l = [left, right]
  where
    (left, right) = splitAt ((length l + 1) `div` 2) l

priority c
  | inRange (ord 'a', ord 'z' + 1) (ord c) = ord c - 96
  | otherwise = ord c - ord 'A' + 27

main = do
  -- _data <- readFile "../inputs/3.sample"
  _data <- readFile "../inputs/3.txt"
  print $ prioritySums $ join $ map (intersection . splitHalf) $ lines _data
  print $ prioritySums $ join $ map (intersection . F.toList) $ F.toList $ Seq.chunksOf 3 $ Seq.fromList $ lines _data
  where
    prioritySums = sum . map priority

-- print $ ""