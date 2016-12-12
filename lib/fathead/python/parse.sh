#!/bin/bash

LC_ALL=C
export PYTHONIOENCODING=utf8
python3 parse.py
python3 redirect.py
mv output2.txt output.txt
sort output.txt -o output.txt

cat output.txt | ./output_statistics.awk
