#!/bin/bash
perl parse.pl download/$(cat data.url | cut -d/ -f6) output.txt

LC_ALL=C sort output.txt -o output.txt
