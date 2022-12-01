import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import Utils (getDayInput)

{-
Methodology:

A naive implementation is to use an actual list of timers which will work for part one

Part two requires something more clever. In this case, I use an array with the indices
as the timer value and the value at that index is the number of fish
 -}

sampleData :: [Int]
sampleData = [3, 4, 3, 1, 2]

type FishStats = S.Seq Int

fishStats :: [Int] -> FishStats
fishStats fish = initial
  where
    initial = S.mapWithIndex (\index _ -> length $ filter (== index) fish) $ S.fromList $ replicate 9 0

next :: FishStats -> FishStats
next fish = after
  where
    toReproduce = fish `S.index` 0
    before = S.drop 1 fish S.>< S.fromList [0]
    after = S.adjust' (+ toReproduce) 6 $ S.adjust' (const toReproduce) 8 before

simulate :: Int -> FishStats -> FishStats
simulate 0 fish = fish
simulate iterations fish = simulate (iterations - 1) (next fish)

main :: IO ()
main = do
  input' <- map T.unpack . T.splitOn (T.pack ",") . T.pack <$> getDayInput 6
  -- let input = sampleData
  let input = map read input' :: [Int]
  print $ sum $ simulate 80 $ fishStats input
  print $ sum $ simulate 256 $ fishStats input