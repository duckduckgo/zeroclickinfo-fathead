#!/bin/bash

python parse.py download/download.tsv output.txt
LC_ALL=C sort output.txt -o output.txt
