#!/bin/sh

date
rm -rf errors.txt output.txt # new content would be appended otherwise
python3 parse.py apple_data/
echo "fin."
