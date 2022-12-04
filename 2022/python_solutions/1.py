data = open('../inputs/1.txt').read()

print(max(sum(map(int, x.splitlines())) for x in data.split("\n\n")))
print(sum(sorted(sum(map(int, x.splitlines())) for x in data.split("\n\n"))[-3:]))
