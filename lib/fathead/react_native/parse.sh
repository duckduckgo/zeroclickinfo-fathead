#!/bin/bash

LC_ALL=C
export PYTHONIOENCODING=utf8
python3 parse.py

LC_ALL=C sort -u output.txt -o output.txt

cat output.txt | ./output_statistics.awk

