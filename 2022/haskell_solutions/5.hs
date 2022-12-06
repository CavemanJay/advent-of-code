{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt, isDigit, ord)
import Data.Foldable qualified as F
import Data.List (sort, transpose)
import Data.Map ((!))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Instruction = Instruction Int Int Int deriving (Show)

parseInstruction line = ins $ map (read . T.unpack) $ filter (isJust . isNumeric . T.unpack) $ T.splitOn " " $ T.pack line
  where
    isNumeric x = readMaybe x :: Maybe Int
    ins [x, y, z] = Instruction x y z

parseColumn x = (digitToInt key, rest)
  where
    (key : rest) = T.unpack x

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

xs !@ (from, to) = slice from to xs

apply (Instruction amount src dest) sameOrder p = newPic
  where
    l = length $ p ! src
    moving = (if sameOrder then reverse else id) $ (p ! src) !@ ((-1) * amount `mod` l, l)
    newPic = M.insert src (pop amount (p ! src)) $ M.insertWith (flip (++)) dest moving p
    pop n = reverse . drop n . reverse

solve pic [] sameOrder = map last $ M.elems pic
solve pic (i : remaining) sameOrder = solve (apply i sameOrder pic) remaining sameOrder

main = do
  _data <- lines <$> readFile "../inputs/5.txt"
  let instructions = map parseInstruction $ tail $ dropWhile (startsWith 'm') _data
  let pic = M.fromList $ map (parseColumn . T.strip) $ filter isColumn $ map (T.reverse . T.pack) $ transpose $ takeWhile (\x -> not $ null x) _data
  print $ solve pic instructions True
  print $ solve pic instructions False
  where
    isColumn = isDigit . T.head
    startsWith c str = not (null str) && head str /= c
