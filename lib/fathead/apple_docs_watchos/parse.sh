#!/bin/bash

rm output.txt
python parse.py
LC_ALL=C sort output.txt -o output.txt
