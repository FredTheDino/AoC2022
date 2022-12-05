import sys

def transpose(l):
    return [reversed(list(i)) for i in zip(*l)]

moves = []
crates_t = []
for line in sys.stdin.readlines():
    if line.startswith("move"):
        moves.append(tuple(map(int, line.split()[1::2])))
    elif len(line) > 30:
        try: crates_t.append(list(line[1:34:4]));
        except IndexError: pass;

moves = [(nr, fr - 1, to - 1) for (nr, fr, to) in moves]
crates = list(map(lambda x: list(filter(lambda y: y != " ", x)), transpose(crates_t)))

def p1(crates):
    for (nr, fr, to) in moves:
        crates[to] += crates[fr][-nr:][::-1]
        crates[fr] = crates[fr][:-nr]

    print("".join(x[-1] for x in crates))

p1(list(map(list, crates)))

def p2(crates):
    for (nr, fr, to) in moves:
        crates[to] += crates[fr][-nr:]
        crates[fr] = crates[fr][:-nr]

    print("".join(x[-1] for x in crates))

p2(list(map(list, crates)))
