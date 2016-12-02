#!/bin/bash

(cd ../python; ./parse.sh)
#run redirect on python2 results to get permutations of redirects, and remove
# duplicates
cp ../python/output_py2.txt ../python/output.txt
(cd ../python; python redirect.py)
cp ../python/output2.txt output.txt
