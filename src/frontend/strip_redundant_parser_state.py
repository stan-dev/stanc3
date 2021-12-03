#!/usr/bin/python3
import re
import sys


matcher = re.compile(
    r"((program|functions_only):.*\n)"
    r"(##.*\n)"
    r"(## Concrete.*\n)?"
    r"(##.*\n)+"
    r"((program|functions_only):.*\n)"
)


def strip_redundant_parser_states(s):
    return matcher.sub(
        lambda m: m.group(1) + m.group(4) + m.group(6),
        s,
    )


def strip_lines(s):
    return "\n".join(map(lambda s: s.rstrip(), s.splitlines()))


if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        messages = f.read()
    curr = strip_redundant_parser_states(messages)
    while messages != curr:
        messages = curr
        curr = strip_redundant_parser_states(messages)
    print(strip_lines(curr))
