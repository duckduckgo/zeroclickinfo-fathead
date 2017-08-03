#!/bin/sh

date
python parse.py data/
sort -u -o output.txt output.txt
echo "fin."
