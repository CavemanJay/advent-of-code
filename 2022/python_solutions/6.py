from typing import Iterable


data = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
data = "bvwbjplbgvbhsrlpgdmjqwftvncz"
data = open('../inputs/6.txt')


def window(seq: Iterable, n):
    for i in range(len(seq) - n + 1):
        yield seq[i: i + n]


def solve(n: int):
    return next(str((i+n, x)) for i, x in enumerate(window(data, n)) if len(set(x)) == n)


print("\n".join(solve(n) for n in [4, 14]))
