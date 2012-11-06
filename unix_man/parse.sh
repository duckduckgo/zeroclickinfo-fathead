#!/usr/bin/env bash
perl ./parse.pl > output.txt

perl -i.bk -pe 's/[^[:ascii:]]+/ /g;' output.txt
