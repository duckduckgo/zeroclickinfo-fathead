#!/usr/bin/bash

perl parse.pl
echo "Sorting output.txt..."
LC_ALL=C sort output.txt -o output.txt

LC_ALL=C sort output.txt -o output.txt
