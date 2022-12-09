from functools import reduce
from operator import mul
sample = False
data = open('../inputs/8' + (".sample" if sample else ".txt")).read()

trees = [list(map(int, x)) for x in data.splitlines()]


def nirange(i, start, stop):
    return (x for x in range(start, stop) if x != i)


def edge_views(i, stop):
    return list(x for x in range(0, i)), list(x for x in range(i+1, stop))


rows = len(trees)
cols = len(trees[0])


def scenic_score(r, c):
    current_tree = trees[r][c]
    top_view, bottom_view = edge_views(r, rows)
    left_view, right_view = edge_views(c, cols)
    top, bottom = [trees[r][c]
                   for r in top_view], [trees[r][c] for r in bottom_view]
    left, right = [trees[r][c]
                   for c in left_view], [trees[r][c] for c in right_view]

    def visible(l):
        for tree in l:
            if tree < current_tree:
                yield tree
            else:
                yield tree
                return
    scores = map(lambda x: len(list(visible(x))),
                 [reversed(top), reversed(left), right, bottom])
    return reduce(mul, scores, 1)


def check(r, c):
    val = trees[r][c]
    top_view, bottom_view = edge_views(r, rows)
    left_view, right_view = edge_views(c, cols)
    top, bottom = [trees[r][c]
                   for r in top_view], [trees[r][c] for r in bottom_view]
    left, right = [trees[r][c]
                   for c in left_view], [trees[r][c] for c in right_view]
    from_top = not any(tree >= val for tree in top)
    from_bottom = not any(tree >= val for tree in bottom)
    from_left = not any(tree >= val for tree in left)
    from_right = not any(tree >= val for tree in right)
    return 1 if any([from_top, from_bottom, from_left, from_right]) else 0


visible = 0
for r in range(rows):
    for c in range(cols):
        if r in [0, rows-1] or c in [0, cols-1]:
            visible += 1
            continue
        visible += check(r, c)
print(visible)

scores = []
for r in range(rows):
    for c in range(cols):
        scores.append(scenic_score(r, c))

# print(scenic_score(1, 2))
# print(scenic_score(3, 2))
print(max(scores))
