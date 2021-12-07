data Change = Increase | Decrease
  deriving (Show)

type Window = [Int]

isIncrease :: Change -> Bool
isIncrease Increase = True
isIncrease _ = False

sampleData :: [Integer]
sampleData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

transform :: [a] -> [(a, a)]
transform [] = []
transform [x] = []
transform (x : y : rest) = (x, y) : transform (y : rest)

toWindows :: [Int] -> [Window]
toWindows [] = []
toWindows [x] = []
toWindows [x, y] = []
toWindows (x : y : z : rest) = [x, y, z] : toWindows (y : z : rest)

getChange :: Int -> Int -> Change
getChange x y =
  if y > x
    then Increase
    else Decrease

countIncreases :: [Int] -> Int
countIncreases d = length $ filter isIncrease $ map (uncurry getChange) $ transform d

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let inputData = map read input :: [Int]
  let partOne = countIncreases inputData
  let partTwo = countIncreases $ map sum $ toWindows inputData
  print partOne
  print partTwo