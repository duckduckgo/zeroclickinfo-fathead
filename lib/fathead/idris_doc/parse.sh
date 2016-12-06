#!/usr/bin/env sh
rm -f output.txt && perl parse.pl

LC_ALL=C sort output.txt -o output.txt
