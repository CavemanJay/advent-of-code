{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text qualified as T

-- data Assignment = Assignment {start :: Int, stop :: Int} deriving (Show)
data Assignment = Assignment Int Int deriving (Show)

start (Assignment start _) = start

stop (Assignment _ stop) = stop

type Pair = (Assignment, Assignment)

parseInt :: String -> Int
parseInt = read

contains x y = (start x <= start y) && (stop y <= stop x)

overlaps x y = not $ (stop x < start y) || (stop y < start x)

parseAssignment x = Assignment start stop
  where
    [start, stop] = map (read . T.unpack) $ T.splitOn "-" x

main = do
  --   pairs <- T.lines . T.pack <$> readFile "../inputs/4.sample"
  pairs <- T.lines . T.pack <$> readFile "../inputs/4.txt"
  print $ solve contains pairs
  print $ solve overlaps pairs
  where
    solve f = length . filter (\[x, y] -> f x y || f y x) . map (map parseAssignment . T.splitOn ",")