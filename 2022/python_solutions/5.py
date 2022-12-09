import parsec
from dataclasses import dataclass
from typing import Dict, List


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


def pic_parser():
    def convert(x):
        pic = [[crate for crate in reversed(y) if crate] for y in zip(*x)]
        pic = {key: pic[key-1] for key in range(1, len(pic)+1)}
        return pic
    no_crate = parsec.parsecmap(parsec.string("   "), lambda _: None)
    crate = parsec.string("[") >> parsec.any() << parsec.string("]")
    opt_crate = parsec.choice(no_crate, crate)
    line = parsec.sepBy1(opt_crate, parsec.string(" "))
    lines = parsec.parsecmap(parsec.sepBy1(line, parsec.string("\n")), convert)

    return lines


def instructions_parser():
    any_line = parsec.regex(r'.*\n')
    num = parsec.parsecmap(parsec.many1(parsec.digit()),
                           lambda x: int("".join(x)))
    line = parsec.parsecmap((
        (parsec.string("move ") >> num) +
        (parsec.string(" from ") >> num) +
        (parsec.string(" to ") >> num)), lambda x: Instruction(*(list(x[0])+[x[1]])))
    return parsec.times(any_line, 3) >> parsec.sepBy1(line, parsec.space())


pic, instructions = pic_parser().parse_partial(data)
instructions = instructions_parser().parse(instructions)

######## ORIGINAL IMPERITIVE SOLUTIONS ########

# def apply(instruction: Instruction):
#     pic[instruction.dest].extend(
#         pic[instruction.src][-1*instruction.amount:][::-1])
#     pic[instruction.src] = pic[instruction.src][:-1*instruction.amount]
# def apply_v2(instruction: Instruction):
#     pic[instruction.dest].extend(pic[instruction.src][-1*instruction.amount:])
#     pic[instruction.src] = pic[instruction.src][:-1*instruction.amount]
# for i in instructions:
# apply(i)
# apply_v2(i)


######## OG FUNCTIONAL RECURSION ########


def solve(pic: Dict, instructions: List[Instruction], same_order: bool):
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
