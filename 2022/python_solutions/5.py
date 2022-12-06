import re
from dataclasses import dataclass
from typing import Dict, List

PicType = Dict[str, List[str]]


@dataclass
class Instruction:
    amount: int
    src: int
    dest: int


data = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

data = open('../inputs/5.txt').read()


class JayStr:
    """Dumb utility class for method chaining regex substitutions"""
    val: str

    def __init__(self, val: str) -> None:
        self.val = val

    def re_sub(self, pattern: str, sub: str):
        self.val = re.sub(pattern, sub, self.val)
        return self


def parse_pic(pic: str):
    vals = JayStr(pic)\
        .re_sub(r'\s?(\s{3})', ',')\
        .re_sub(r' ', ',')\
        .re_sub(r'[\[\]]', ' ')\
        .val.replace(" ", '').splitlines()
    vals = [x.split(',') for x in reversed(vals)][1:]
    keys = range(1, len(vals[0])+1)

    def extract(key: int):
        return list(x[key-1] for x in vals if x[key-1])
    return {key: extract(key) for key in keys}


def get_steps(instructions: str):
    return [
        Instruction(*map(int, (x for x in line.split(' ') if x.isdigit())))
        for line in instructions.splitlines()
    ]


pic, instructions = data.split("\n\n")
pic = parse_pic(pic)
instructions = get_steps(instructions)

######## ORIGINAL IMPERITIVE SOLUTIONS ########

# def apply(instruction: Instruction):
#     pic[instruction.dest].extend(
#         pic[instruction.src][-1*instruction.amount:][::-1])
#     pic[instruction.src] = pic[instruction.src][:-1*instruction.amount]
# def apply_v2(instruction: Instruction):
#     pic[instruction.dest].extend(pic[instruction.src][-1*instruction.amount:])
#     pic[instruction.src] = pic[instruction.src][:-1*instruction.amount]
# for i in instructions:
#     apply(i)
#     apply_v2(i)

######## OG FUNCTIONAL RECURSION ########


def solve(pic: PicType, instructions: List[Instruction], same_order: bool):
    if not instructions:
        return "".join(x[-1] for x in pic.values())
    i = instructions[0]
    src, dest = i.src, i.dest
    _order = 1 if same_order else -1
    return solve({
        **pic,
        dest: pic[dest]+pic[src][-1*i.amount:][::_order],
        src: pic[src][:-1*i.amount]
    }, instructions[1:], same_order)


print(solve(pic, instructions, False))
print(solve(pic, instructions, True))
