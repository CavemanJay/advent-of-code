from dataclasses import dataclass

data = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

data = open("../inputs/4.txt").read()


@dataclass
class Assignment:
    start: int
    stop: int

    def contains(self, other: 'Assignment'):
        return self.start <= other.start and other.stop <= self.stop

    def overlaps(self, other: 'Assignment'):
        # my solution
        # return self.contains(other) or any(x for x in range(self.start, self.stop+1) if x in range(other.start, other.stop + 1))

        # Solution from https://youtu.be/15qPSEFoR0U?t=202
        return not (self.stop < other.start or other.stop < self.start)

    @staticmethod
    def parse(s: str):
        return Assignment(*map(int, s.split('-')))


assignments = [[Assignment.parse(x) for x in pair.split(",")]
               for pair in data.splitlines()]


def check(func): return lambda pair: func(
    pair[0], pair[1]) or func(pair[1], pair[0])


print(len(list(filter(check(Assignment.contains), assignments))))
print(len(list(filter(check(Assignment.overlaps), assignments))))
