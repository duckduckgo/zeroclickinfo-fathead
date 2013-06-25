#!/bin/bash

# we can use glob since the archive contains a single directory
pushd download/*/ > /dev/null
python3 index2ddg.py index-functions-cpp.xml ../../output.txt
popd > /dev/null

