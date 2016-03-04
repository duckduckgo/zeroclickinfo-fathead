#!/bin/bash
python parse.py --out output.txt \
                --langdefs ecma.csv \
                --cachedir downloads \
                --cachejournal ".cachejournal"
