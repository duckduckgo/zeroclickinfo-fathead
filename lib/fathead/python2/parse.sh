#!/bin/bash

LC_ALL=C
export PYTHONIOENCODING=utf8
(cd ../python; python3 parse.py)
# Use the python2 entries as input to redirect.py
cp ../python/output_py2.txt ../python/output.txt
(cd ../python; python3 redirect.py)
cp ../python/output2.txt output.txt
sort output.txt -o output.txt
cat output.txt | ../python/output_statistics.awk
