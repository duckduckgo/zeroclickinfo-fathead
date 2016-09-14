#!/bin/bash
rm output.txt
python parse_functions.py
python parse_reference.py
LC_ALL=C sort output.txt -o output.txt
