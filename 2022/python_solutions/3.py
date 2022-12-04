from typing import Any, List


# data = """vJrwpWtwJgWrhcsFMMfFFhFp
# jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
# PmmdzqPrVvPwwTWBwg
# wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
# ttgJtRGJQctTZtZT
# CrZsJsPPZsGzwwsLwLmpwMDw
# """

data = open('../inputs/3.txt').read()


def split(s: str):
    half = len(s)//2
    return s[:half], s[half:]


def chunk(n: int, l: List[Any]):
    return [l[i * n:(i + 1) * n] for i in range((len(l) + n - 1) // n)]


def get_common(*args):
    return set(args[0]).intersection(*args[1:])


def get_score(c: str):
    return ord(c)-96 \
        if ord(c) in range(ord('a'), ord('z')+1) \
        else ord(c) - ord('A') + 27


print(sum(
    get_score(y)
    for x in data.splitlines()
    for y in get_common(*split(x))
))

print(sum(
    get_score(badge)
    for group in chunk(3, data.splitlines())
    for badge in get_common(*group)
))


# print(sum(y for x in data.splitlines() for y in list(map(lambda s: ord(s)-96 if ord(s) in range(ord('a'), ord('z')+1) else ord(s) - ord('A') + 27, set(c for c in x[:len(x)//2] if c in x[len(x)//2:])))))
