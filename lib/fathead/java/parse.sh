#!/bin/bash

#sudo apt-get install python-beautifulsoup4
python3 parse.py

LC_ALL=C

if [[ -e "output.txt" && -e "cover/methods.txt" ]]; then
    sort -u output.txt -o output.txt
    sort -u cover/methods.txt -o cover/methods.txt
fi
