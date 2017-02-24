#!/bin/bash                                                                                                                                

python3 parse.py
LC_ALL=C sort output.txt -o output.txt
