#!/bin/bash
perl parse.pl download/hgnc_complete_dataset.tsv.gz output.txt


LC_ALL=C sort output.txt -o output.txt
