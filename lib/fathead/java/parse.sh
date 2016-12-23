#!/bin/bash

#sudo apt-get install python-beautifulsoup4
python parse.py
LC_ALL=C sort output.txt -o output.txt
