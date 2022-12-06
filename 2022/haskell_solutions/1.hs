{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text qualified as T

main = do
  _data <- T.pack <$> readFile "../inputs/1.txt"
  print $ maximum $ map (sum . map read . lines . T.unpack) $ T.splitOn "\n\n" _data
  print $ sum $ take 3 $ reverse . sort $ map (sum . map read . lines . T.unpack) $ T.splitOn "\n\n" _data