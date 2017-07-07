#!/bin/sh

date
python3 parse.py apple_data/
sort -u -o output.txt output.txt
echo "fin."