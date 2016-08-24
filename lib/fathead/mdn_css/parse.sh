#!/usr/bin/bash

perl parse.pl
echo "Sorting output.txt..."
sort output.txt -o output.txt
