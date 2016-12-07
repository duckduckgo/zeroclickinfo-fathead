#!/bin/bash

export PYTHONIOENCODING=utf8
(cd ../python; ./parse.sh)
#Run redirect on python2 results to get permutations of redirects, and remove
# duplicates
cp ../python/output_py2.txt ../python/output.txt
(cd ../python; python redirect.py)
cp ../python/output2.txt output.txt
echo "Python 2 output contains"
cat output.txt | ../python/output_statistics.awk
