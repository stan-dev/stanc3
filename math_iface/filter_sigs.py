import re
import sys

stdlib_fns = [
    "is_nan(real)", "abs(int)", "abs(real)", "add(int, int)", "add(real, real)",
    "add(int)", "add(real)", "add(vector)", "add(matrix)", "atan2(real, real)"
]
stdlib_fn_re_pattern = "|".join("(.*" + re.escape(s) + ".*)" for s in stdlib_fns)
stdlib_fn_re = re.compile(stdlib_fn_re_pattern)
def is_stdlib_fn(line):
    return re.search(stdlib_fn_re, line)

def no_spaces(s):
    return s.group().replace(" ", "")

TEMP_OK_FNS = ["normal", "add"]


if __name__ == "__main__":
    for line in sys.stdin:
        line = re.sub('\[[^\]]*\]', no_spaces, line)
        if "real[" in line:
            continue
        if "[," in line:
            continue
        if line.count(",") > 2:
            continue
        if "row_vector" in line:
            continue
        if "_rng" in line: # XXX
            continue
        if is_stdlib_fn(line):
            continue
        found = False
        for fn in TEMP_OK_FNS:
            if fn in line:
                found = True
                break
        if not found:
            continue
        print line,
