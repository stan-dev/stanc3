#!/usr/bin/python3


import re
import subprocess
import sys
parse = re.compile('File "(.*)", line (\d+).*\nError: this sentence.*\nNo sentence that leads to this state exists in "(.*)".')

parser = sys.argv[1]
new_messages = sys.argv[2]
old_messages = sys.argv[3]

p = subprocess.run("menhir {} --compare-errors {} --compare-errors {}".format(
    parser, new_messages, old_messages),
                   shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)


updates = parse.findall(p.stderr.decode("utf-8"))

for new_file, line_no, file_to_update in updates:
    line_no = int(line_no)
    new_lines = ["\n", "\n"]
    with open(new_file) as new:
        for line in new:
            line_no-=1
            if line_no == 0:
                new_lines.append(line)
            elif line_no < 0:
                if line.startswith("##"):
                    new_lines.append(line)
                else:
                    break

    new_lines.append("\nTODO: PARSER MESSAGE NEEDED HERE.")
    with open(file_to_update, "a") as f:
        f.writelines(new_lines)

print("Added %d message stubs."% len(updates))
