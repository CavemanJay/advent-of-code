module Utils where

import Debug.Trace (traceShow)
import Text.Printf (printf)

getDayInput :: Int -> IO String
getDayInput day = readFile $ printf "resources/day %d input.txt" day

traceShow' :: Show p => Bool -> p -> p
traceShow' debug arg =
  if debug
    then traceShow arg arg
    else arg

-- https://stackoverflow.com/a/23660388
fmapNested :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmapNested = fmap . fmap