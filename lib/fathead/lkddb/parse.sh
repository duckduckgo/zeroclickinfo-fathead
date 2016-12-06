#!/bin/bash

cd $(dirname -- "$0")

python3 parse.py

LC_ALL=C sort output.txt -o output.txt
