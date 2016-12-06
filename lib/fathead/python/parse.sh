#!/bin/bash

export PYTHONIOENCODING=utf8
python parse.py
python redirect.py
mv output2.txt output.txt

LC_ALL=C sort output.txt -o output.txt
