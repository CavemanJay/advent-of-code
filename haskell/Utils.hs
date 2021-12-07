module Utils where
import Text.Printf (printf)

getDayInput :: Int -> IO String
getDayInput day = readFile $ printf "resources/day %d input.txt" day