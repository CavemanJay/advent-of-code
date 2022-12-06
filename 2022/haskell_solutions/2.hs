{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T

hand1 x =
  fromJust $
    M.lookup x $
      M.fromList
        [ ("X", 1),
          ("Y", 2),
          ("Z", 3)
        ]

hand2 x =
  fromJust $
    M.lookup x $
      M.fromList
        [ ("X", 0),
          ("Y", 3),
          ("Z", 6)
        ]

score1 x =
  fromJust $
    M.lookup x $
      M.fromList
        [ (["A", "X"], 3),
          (["A", "Y"], 6),
          (["A", "Z"], 0),
          (["B", "X"], 0),
          (["B", "Y"], 3),
          (["B", "Z"], 6),
          (["C", "X"], 6),
          (["C", "Y"], 0),
          (["C", "Z"], 3)
        ]

score2 x =
  fromJust $
    M.lookup x $
      M.fromList
        [ (["A", "X"], 3),
          (["A", "Y"], 1),
          (["A", "Z"], 2),
          (["B", "X"], 1),
          (["B", "Y"], 2),
          (["B", "Z"], 3),
          (["C", "X"], 2),
          (["C", "Y"], 3),
          (["C", "Z"], 1)
        ]

calcRoundScore1 round = hand1 me + score1 [opp, me]
  where
    [opp, me] = T.splitOn " " round

calcRoundScore2 round = hand2 me + score2 [opp, me]
  where
    [opp, me] = T.splitOn " " round

main = do
  _data <- T.lines . T.pack <$> readFile "../inputs/2.txt"
  print $ sum $ map calcRoundScore1 _data
  print $ sum $ map calcRoundScore2 _data
