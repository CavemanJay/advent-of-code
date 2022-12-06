{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List (sort, tails)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable (sequenceA)

-- https://stackoverflow.com/a/27733778
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

-- End stolen code

check n xs = n + fst (head $ filter check' wins)
  where
    wins = zip [0 ..] $ windows n xs
    check' x = n == length (S.fromList $ snd x)

main = do
--   _data <- readFile "../inputs/6.sample"
  _data <-  readFile "../inputs/6.txt"
  print $ check 4 _data
  print $ check 14 _data
