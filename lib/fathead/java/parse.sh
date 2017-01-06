#!/bin/bash

#sudo apt-get install python-beautifulsoup4
python3 parse.py

if [[ -e "output.txt" && -e "cover/methods.txt" ]]; then
    LC_ALL=C sort output.txt -o output.txt
    LC_ALL=C sort cover/methods.txt -o cover/methods.txt
fi
