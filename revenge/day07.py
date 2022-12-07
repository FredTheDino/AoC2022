import sys

folder_sizes = []

def folder_size(stack, lines):
    global folder_sizes

    size = 0
    total = 0

    if not lines: return size, total, []

    while True:
        if not lines: break

        [line, *lines] = lines
        line = line.strip().split()
        if not line: continue

        if line[0] == "$" and line[1] == "cd" and line[2] == "..":
            break

        elif line[0] == "$" and line[1] == "cd":
            inner, inner_total, lines = folder_size(stack + [line[2]], lines)
            total += inner_total
            size += inner

        elif line[0] == "$" and line[1] == "ls":
            pass

        elif line[0] == "dir":
            pass

        else:
            size += int(line[0])

    if size < 100000:
        total += size

    folder_sizes.append((size, stack[-1]))
    return size, total, lines

lines = sys.stdin.readlines()
(total, wierd_sum, _) = folder_size(["/"], lines[1:])

print(wierd_sum)
for (s, f) in sorted(folder_sizes):
    if 70000000 - total + s > 30000000:
        print(f, s)
        break
