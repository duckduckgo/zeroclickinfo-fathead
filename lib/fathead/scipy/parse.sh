#!/bin/bash

python parse.py scipy-docs/
sort output.txt -o output.txt
