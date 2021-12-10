{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import Utils

debug :: Bool
debug = False

ts :: Show p => p -> p
ts = traceShow' True

size = 5

data MarkableInt = Marked Int | Unmarked Int
  deriving (Show)

value :: MarkableInt -> Int
value i = case i of
  Marked x -> x
  Unmarked x -> x

isMarked :: MarkableInt -> Bool
isMarked (Marked _) = True
isMarked _ = False

-- mark :: (MarkableInt -> MarkableInt) (MarkableInt -> MarkableInt) -> MarkableInt -> MarkableInt
-- mark whenMarked whenUnmarked i =
--   if isMarked i
--     then whenMarked i
--     else whenUnmarked i

type Board = [[MarkableInt]]

sampleData :: T.Text
sampleData =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \6 10  3 18  5\n\
  \1 12 20 15 19\n\
  \\n\
  \3 15  0  2 22\n\
  \9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \2  0 12  3  7"

sampleBoard =
  [ [Unmarked 14, Unmarked 21, Unmarked 17, Unmarked 24, Unmarked 4],
    [Unmarked 10, Unmarked 16, Unmarked 15, Unmarked 9, Unmarked 19],
    [Unmarked 18, Unmarked 8, Unmarked 23, Unmarked 26, Unmarked 20],
    [Unmarked 22, Unmarked 11, Unmarked 13, Unmarked 6, Unmarked 5],
    [Unmarked 2, Unmarked 0, Unmarked 12, Unmarked 3, Unmarked 7]
  ]

sampleBoard' =
  [ [Marked 14, Marked 21, Marked 17, Marked 24, Marked 4],
    [Unmarked 10, Unmarked 16, Unmarked 15, Marked 9, Unmarked 19],
    [Unmarked 18, Unmarked 8, Marked 23, Unmarked 26, Unmarked 20],
    [Unmarked 22, Marked 11, Unmarked 13, Unmarked 6, Marked 5],
    [Marked 2, Marked 0, Unmarked 12, Unmarked 3, Marked 7]
  ]

textBoard :: T.Text -> [[T.Text]]
textBoard = filter ((> 1) . length) . map (T.splitOn " " . T.strip) . T.splitOn "\n"

board :: T.Text -> [[MarkableInt]]
board = fmapNested Unmarked . fmapNested (read . T.unpack) . textBoard

markBoard :: Int -> Board -> Board
markBoard val = fmapNested update
  where
    update v =
      if value v == val
        then Marked $ value v
        else v

hasWon :: Board -> Bool
hasWon board = rowWin board || colWin board
  where
    rowWin = any ((== size) . length . traceShow' debug . filter isMarked)
    colWin = rowWin . transpose

findWinningBoard :: [Int] -> [Board] -> Board
findWinningBoard [] _ = undefined
findWinningBoard _ [b] = b
findWinningBoard (next : rest) boards =
  if length won > 0
    then head won
    else findWinningBoard rest (map (markBoard next) boards)
  where
    won = filter hasWon boards

boardSum board = sum $ map value $ filter (not . isMarked) $ concat board

main :: IO ()
main = do
  input' <- T.pack <$> getDayInput 4
  let input = input'
  let order = map (read . T.unpack) $ T.splitOn "," $ head $ T.lines input :: [Int]
  let boardsText = T.replace "  " " " $ T.unlines $ tail $ tail $ T.lines input
  -- print boardsText
  let boards = map board $ T.splitOn "\n\n" boardsText
  -- let boards = map textBoard $ T.splitOn "\n\n" boardsText
  -- let boards' = [last boards]
  -- print boards

  -- let orders = take 13 order -- Should be a win condition after 12
  let b = findWinningBoard order boards
  print $ b
