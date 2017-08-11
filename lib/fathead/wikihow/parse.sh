#!/bin/sh

date
rm -r output.txt
python parse.py data/
sort -u -o output.txt output.txt
echo "fin."
