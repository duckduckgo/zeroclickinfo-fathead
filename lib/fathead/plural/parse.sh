#!/bin/sh

perl extract_plurals.pl
LC_ALL=C sort output.txt -o output.txt
