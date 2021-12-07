import Control.Monad (join)
import Data.Char (digitToInt)
import Data.List (group, sort, transpose)
import Numeric (readInt)
import Utils (getDayInput, traceShow')

data Commonality = Least | Most

debug = False

sampleData = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

commonality :: Commonality -> String -> Int
commonality c digits = case compare zeroCount oneCount of
  EQ -> case c of
    Least -> 0
    Most -> 1
  LT -> case c of
    Least -> 0
    Most -> 1
  GT -> case c of
    Least -> 1
    Most -> 0
  where
    groups = map length $ group $ sort digits
    zeroCount = head groups
    oneCount = last groups

-- Based on: https://stackoverflow.com/a/5922212
readBin :: String -> Integer
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

bitFilter :: Int -> Commonality -> [String] -> String
bitFilter _ _ [x] = x
bitFilter index common d = bitFilter (index + 1) common matching
  where
    keep = head $ show $ commonality common $ transpose d !! index
    matching = traceShow' debug $ filter (\x -> x !! index == keep) d

main = do
  input' <- lines <$> getDayInput 3
  print $ partOne input'
  print $ partTwo input'
  where
    gamma = readBin . join . map (show . commonality Most) . transpose
    epsilon = readBin . join . map (show . commonality Least) . transpose
    partOne input = epsilon input * gamma input

    oxygenRating = readBin . bitFilter 0 Most
    co2Rating = readBin . bitFilter 0 Least
    partTwo input = oxygenRating input * co2Rating input