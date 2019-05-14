import sys
import re

with open(sys.argv[1]) as f:
    lines = f.readlines()[6:-6]
    for l in lines:
        cols = re.split("\s+", l)[0:7]
        print(("{: >14} " * len(cols)).format(*cols))
