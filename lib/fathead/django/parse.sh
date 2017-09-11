#!/bin/bash

rm -f output.txt
python3 parse.py
python3 redirect.py

sort -u -t$'\t' -k1,1 < output2.txt > output_sorted.txt
mv output_sorted.txt output.txt
cat output.txt | ./output_statistics.awk
