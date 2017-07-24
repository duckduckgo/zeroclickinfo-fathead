#!/bin/sh

date
python parse.py data/html
sort -u -o output.txt output.txt
echo "fin."
