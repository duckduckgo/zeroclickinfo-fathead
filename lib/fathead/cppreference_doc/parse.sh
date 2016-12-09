#!/bin/bash

# we can use glob since the archive contains a single directory
python3 parse.py ./download/*/index-functions-cpp.xml output.txt