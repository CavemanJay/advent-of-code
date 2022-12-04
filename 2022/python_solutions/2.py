# Credit to: https://www.youtube.com/watch?v=X1XH774hId0
# I did not come up with the score calculation on my own


data = open('../inputs/2.txt').read()

# data = """A Y
# B X
# C Z"""


def calc_round_score(round: str):
    opp, me = round.split(' ')
    score = {"X": 1, "Y": 2, "Z": 3}[me] + {
        ('A', "X"): 3, ('A', "Y"): 6, ('A', "Z"): 0,
        ('B', "X"): 0, ('B', "Y"): 3, ('B', "Z"): 6,
        ('C', "X"): 6, ('C', "Y"): 0, ('C', "Z"): 3,
    }[(opp, me)]
    return score

# X means you need to lose, 
# Y means you need to end the round in a draw, 
# Z means you need to win.

def calc_round_score_v2(round: str):
    opp, me = round.split(' ')
    score = {"X": 0, "Y": 3, "Z": 6}[me] + {
        ('A', "X"): 3,('A', "Y"): 1,('A', "Z"): 2,
        ('B', "X"): 1,('B', "Y"): 2,('B', "Z"): 3,
        ('C', "X"): 2,('C', "Y"): 3,('C', "Z"): 1,
    }[(opp, me)]
    return score


print(sum(map(calc_round_score, data.splitlines())))
print(sum(map(calc_round_score_v2, data.splitlines())))
