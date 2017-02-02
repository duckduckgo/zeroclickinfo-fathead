#!/bin/bash

LC_ALL=C
export PYTHONIOENCODING=utf8
python3 parse.py
python3 redirect.py

LC_ALL=C sort -u -t$'\t' -k1,1 output2.txt -o output.txt

cat output.txt | ./output_statistics.awk

