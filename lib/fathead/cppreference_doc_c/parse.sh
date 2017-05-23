#!/bin/bash

# we can use glob since the archive contains a single directory
pushd download/*/ > /dev/null
python3 index2ddg.py index-functions-c.xml ../../output.txt
popd > /dev/null
LC_ALL=C sort output.txt -o output.txt
