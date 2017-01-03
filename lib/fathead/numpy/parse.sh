#!/bin/bash

python parse.py numpy-docs/
sort output.txt -o output.txt
