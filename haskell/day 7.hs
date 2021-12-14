{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Utils (getDayInput)

sampleData :: [Integer]
sampleData = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

costOfIndex :: Int -> [Int] -> Int
costOfIndex index = sum . map (\x -> abs $ x - index)

costOfIndex' :: Int -> [Int] -> Int
costOfIndex' index = sum . map (move index)

move :: Integral a => a -> a -> a
move index crab = (delta * (delta + 1)) `div` 2
  where
    delta = abs $ crab - index

main :: IO ()
main = do
  input' <- map T.unpack . T.splitOn "," . T.pack <$> getDayInput 7
  -- let input = sampleData
  let input = map read input' :: [Int]
  let positions = [minimum input .. maximum input]
  let initialCosts = M.fromAscList $ zip positions $ repeat 0
  let solve f = M.mapWithKey f initialCosts

  let positionCosts = solve (\index _ -> costOfIndex index input)
  print $ answer positionCosts

  let positionCosts' = solve (\index _ -> costOfIndex' index input)
  print $ answer positionCosts'
  where
    answer costs = minimum $ M.elems costs
