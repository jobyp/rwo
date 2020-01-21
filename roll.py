import sys

def partition(x, cmds):
    lo = 0
    hi = len(cmds) - 1

    if x > cmds[hi]:
        return hi + 1
    elif x < cmds[lo]:
        return lo

    # Binary search since cmds[lo] <= x <= cmds[hi]
    while True:
        if lo >= hi:
            break
        mid = (lo + hi) // 2
        if x < cmds[mid]:
            hi = mid - 1
        elif x > cmds[mid]:
            lo = mid + 1
        else:
            # may be there are duplicates ... find first entry
            hi = mid
    return lo


def roll(s, cmds):
    cmds.sort()
    n = len(cmds)
    l = []

    for i, x in enumerate(s):
        j = partition(i + 1, cmds)
        v = ord(x) - ord('a')
        v = (v + (n - j)) % 26 + ord('a')
        l.append(chr(v))

    return ''.join(l)


def main():
    while True:
        x = sys.stdin.readline()
        if not x:
            break
        _ = sys.stdin.readline()
        s = sys.stdin.readline().strip()
        cmds = [int(i) for i in sys.stdin.readline().split()]
        print(roll(s, cmds))

if __name__ == "__main__":
    main()
    sys.exit(0)



