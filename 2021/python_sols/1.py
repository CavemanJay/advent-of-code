from typing import List


data = """199
200
208
210
200
207
240
269
260
263"""

data = open('../inputs/1.txt').read()


def window(n: int, seq: List):
    for i in range(len(seq) - n + 1):
        yield seq[i: i + n]


def identity(x): return x


def solve(l):
    return sum(filter(identity, (1 if y > x else 0 for x, y in (
        window(2, list(map(int, l)))))))


print(solve(data.splitlines()))
print(solve(list(map(sum, window(3, list(map(int, data.splitlines())))))))
