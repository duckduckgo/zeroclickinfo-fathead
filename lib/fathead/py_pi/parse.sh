#!/bin/bash

python2.7 parse.py

LC_ALL=C sort output.txt -o output.txt
