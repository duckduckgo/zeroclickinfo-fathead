#!/bin/bash
#rm output.txt

python parse.py --out output.txt \
                --langdefs ecma.csv \
                --cachedir downloads \
                --cachejournal ".cachejournal"

#python parse.py --out output.js.txt \
#                --langdefs ecma.csv \
#                --cachedir downloads_js \
#                --cachejournal ".cachejournal"

#cat output.api.txt >> output.txt
#cat output.js.txt >> output.txt
