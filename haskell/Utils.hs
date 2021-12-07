module Utils where
import Text.Printf (printf)
import Debug.Trace (traceShow)

getDayInput :: Int -> IO String
getDayInput day = readFile $ printf "resources/day %d input.txt" day

traceShow' debug arg =
  if debug
    then traceShow arg arg
    else arg