{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Utils

debug :: Bool
debug = False

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

textBoard :: T.Text -> [[T.Text]]
textBoard = traceShow' debug . filter ((> 1) . length) . map (T.splitOn " ") . T.splitOn "\n"

-- board :: T.Text -> [[Int]]
board :: T.Text -> [[MarkableInt]]
board = fmapNested Unmarked . fmapNested (read . T.unpack) . textBoard

markBoard :: Int -> Board -> Board
markBoard val = fmapNested update
  where
    update v =
      if value v == val
        then Marked $ value v
        else v

main :: IO ()
main = do
  -- input' <- lines <$> getDayInput 4
  let order = map (read . T.unpack) $ T.splitOn "," $ head $ T.lines sampleData :: [Int]
  let sampleData' = T.replace "  " " " $ T.unlines $ tail $ tail $ T.lines sampleData
  let boards = map board $ T.splitOn "\n\n" sampleData'
  -- let boards = map textBoard $ T.splitOn "\n\n" sampleData'
  let board1 = head boards
  let updatedBoard1 = markBoard 11 board1
  print $ board1
  print ""
  print $ updatedBoard1
