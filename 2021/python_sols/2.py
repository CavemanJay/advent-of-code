import operator
from functools import reduce
from typing import Literal, Tuple


sample = False
data = open('../inputs/2' + (".sample" if sample else ".txt")).read()

Direction = Literal['up', 'down', 'forward']
Instruction = Tuple[Direction, int]


def parse_instruction(s: str):
    x, y = s.split(' ')
    return x, int(y)


def apply_v1(x: Tuple[int, int], i: Instruction):
    pos, depth = x
    match i:
        case ('up', num):
            return (pos, depth - num)
        case ('down', num):
            return (pos, depth + num)
        case ('forward', num):
            return (pos + num, depth)


def apply_v2(x: Tuple[int, int, int], i: Instruction):
    pos, depth, aim = x
    match i:
        case ('up', num):
            return (pos, depth, aim-num)
        case ('down', num):
            return (pos, depth, aim+num)
        case ('forward', num):
            return (pos + num, depth+aim*num, aim)


ins = list(map(parse_instruction, data.splitlines()))
print(reduce(operator.mul, reduce(apply_v1, ins, (0, 0))))
print(reduce(operator.mul, reduce(apply_v2, ins, (0, 0, 0))[:-1]))
