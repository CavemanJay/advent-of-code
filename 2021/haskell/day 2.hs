import Utils

data Movement = Forward Int | Up Int | Down Int
  deriving (Show)

debug = False

sampleData = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

isDepth (Forward _) = False
isDepth (Up _) = True
isDepth (Down _) = True

value (Forward x) = x
value (Up x) = x
value (Down x) = x

parseLine :: String -> Movement
parseLine line = case words line of
  ["forward", x] -> Forward $ read x
  ["up", x] -> Up $ read x
  ["down", x] -> Down $ read x
  _ -> undefined

calculatePos :: [Movement] -> (Int, Int)
calculatePos moves = (horizontalPos, verticalPos)
  where
    horizontalPos = sum $ map value $ filter (not . isDepth) moves
    verticalPos = calcDepth' $ filter isDepth moves
    calcDepth' [] = 0
    calcDepth' (move : rest) =
      calcDepth' rest + calcDepth move

calcDepth move = case move of
  Up val -> - val
  Down val -> val
  Forward val -> 0

calcDist (Forward x) = x
calcDist _ = 0

calculatePos' :: [Movement] -> (Int, Int)
calculatePos' moves = (h, v)
  where
    (h, v, _) = calc moves (0, 0, 0)
    calc [] pos = pos
    calc (move : rest) pos = calc rest $ nextPos move pos
    nextPos move (h', depth', aim') = case traceShow' debug move of
      Forward x -> traceShow' debug (h' + x, depth' + aim' * x, aim')
      Up d -> traceShow' debug (h', depth', aim' - d)
      Down d -> traceShow' debug (h', depth', aim' + d)

main = do
  input' <- lines <$> getDayInput 2
  let input = map parseLine input'
  --   let input = map parseLine sampleData
  let partOne = uncurry (*) $ calculatePos input
  let partTwo = uncurry (*) $ calculatePos' input
  print $ partOne
  print $ partTwo