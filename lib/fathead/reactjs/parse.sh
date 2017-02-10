#!/bin/bash

python parse.py
LC_ALL=C sort -u -t$'\t' -k1,1 output.txt -o output.txt
