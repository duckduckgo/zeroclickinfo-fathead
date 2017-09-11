#!/usr/bin/env bash

sed -i '/^#/ d' output.txt                        # Remove comments
sed -i 's/.*(//' output.txt                       # Remove columns not needed
sed -i 's|[(),]||g' output.txt                    # Remove brackets
sed -i 's/\(.*[^ ]\)[ ]*\(.*\)/\2 \1/' output.txt # Move first column to last
sed -i '/^$/d' output.txt                         # Remove blank lines

# Break lines with 2 entries into 2 lines
sed -i 's/\.\.\([^ ]*\) *\(.*\)\.\./\t\2\n \1\t/' output.txt


sed -i "s/[[:space:]]\+/ /g" output.txt           # Format spacing
