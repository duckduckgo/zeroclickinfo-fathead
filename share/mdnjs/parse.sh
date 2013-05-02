#!/bin/bash
python parse.py --out output.txt \
                --langdefs propref.csv \
                --cachedir downloads \
                --cachejournal ".cachejournal"
