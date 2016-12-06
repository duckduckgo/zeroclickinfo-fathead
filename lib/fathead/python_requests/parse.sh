#!/usr/bin/bash

python parse.py
sort output.txt -o output.txt

LC_ALL=C sort output.txt -o output.txt
