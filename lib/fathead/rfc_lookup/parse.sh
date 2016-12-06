#!/bin/bash
perl parse.pl < download/rfc-index.xml > output.txt

LC_ALL=C sort output.txt -o output.txt
