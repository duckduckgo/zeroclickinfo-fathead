#!/usr/bin/env bash

sed -i '/^#/ d' output.txt # Remove comments
sed -i 's/.*(//' output.txt # Remove columns not needed
sed -i 's|[(),]||g' output.txt # Remove brackets around emoji
sed -i 's/\(.*[^ ]\)[ ]*\(.*\)/\2 \1/' output.txt # Move first column to last
sed -i '/^$/d' output.txt # remove blank lines

# break lines with 2 entries into 2 lines
sed -i '/.\.\./ s/\(.\)\.\.\(.\)\s*\(.*\)\.\./\1\t\3\n \2\t/' output.txt
