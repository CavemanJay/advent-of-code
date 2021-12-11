{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Printf
import Utils

showCellPos = False

ts x = traceShow' True x

sampleData :: T.Text
sampleData =
  "0,9 -> 5,9\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \8,0 -> 0,8\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2"

type Grid = [[Point]]

data Point = Point {x :: Int, y :: Int}
  deriving (Ord)

instance Eq Point where
  (==) p1 p2 = x p1 == x p2 && y p1 == y p2

instance Show Point where
  show point = printf "(%d,%d)" (x point) (y point)

data Line = Line {start :: Point, end :: Point}
  deriving (Ord, Eq)

instance Show Line where
  show line = printf "(%d,%d) -> (%d,%d)" x1 y1 x2 y2
    where
      x1 = x $ start line
      y1 = y $ start line
      x2 = x $ end line
      y2 = y $ end line

linePoints :: Line -> [Point]
linePoints line = points'
  where
    [start', end'] = sort [start line, end line]
    points' =
      if x start' == x end'
        then [Point (x start') y' | y' <- [y start' .. y end']]
        else [Point x' (y start') | x' <- [x start' .. x end']]

isHorizontal :: Line -> Bool
isHorizontal (Line st nd) = y st == y nd

isVertical :: Line -> Bool
isVertical (Line st nd) = x st == x nd

parseLine :: T.Text -> Line
parseLine s = Line (parsePoint start) (parsePoint end)
  where
    [start, _, end] = T.splitOn " " s

parsePoint :: T.Text -> Point
parsePoint s = Point x y
  where
    [x, y] = map (read . T.unpack) $ T.splitOn "," s

display :: Grid -> String
display [] = ""
display (row : rest) = unwords (map show row) ++ "\n" ++ display rest

getGridSize :: Foldable t => t Line -> Int
getGridSize lines = maximum [maxX, maxY]
  where
    maxX = maximum $ concatMap (\l -> map x [start l, end l]) lines
    maxY = maximum $ concatMap (\l -> map y [start l, end l]) lines

fillGrid :: [Point] -> Map Point [Point] -> Map Point [Point]
fillGrid [] grid = grid
fillGrid (point : rest) grid = fillGrid rest $ M.adjust (point :) point grid

createGrid :: [Point] -> Map Point [Point]
createGrid = M.fromList . map (,[])

main :: IO ()
main = do
  input' <- T.pack <$> getDayInput 5
  -- let input = sampleData
  let input = input'
  let lines' = filter (\x -> isHorizontal x || isVertical x) $ map parseLine $ T.lines input
  let size = getGridSize lines'
  let allPoints = concatMap linePoints lines'
  let cells = [Point x y | x <- [0 .. size], y <- [0 .. size]]
  let grid = fillGrid allPoints $ createGrid cells
  let result = length $ filter (\x -> length x >= 2) $ M.elems grid
  print result
