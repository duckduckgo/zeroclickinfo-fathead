#!/bin/bash

python3 parse.py > output.txt
LC_ALL=C sort output.txt -o output.txt
