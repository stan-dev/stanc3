#!/usr/bin/python3
# Run this from src/frontend with dune on the path

import os.path
import re
import subprocess
parse = re.compile('File "(.*)", line (\d+).*\nError: this sentence.*\nNo sentence that leads to this state exists in "(.*)".')

p = subprocess.run("dune runtest .", shell=True, capture_output=True)
# TODO: We can read the build dir from the first line of the stderr of dune runtest

build_dir = "../../_build/default/src/frontend/"

updates = parse.findall(p.stderr.decode("utf-8"))

for new_file, line_no, file_to_update in updates:
    line_no = int(line_no)
    new_lines = ["\n", "\n"]
    with open(os.path.join(build_dir, new_file)) as new:
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
