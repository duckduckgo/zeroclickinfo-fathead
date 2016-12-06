#!/bin/bash

python parse.py --django-version 1.9

LC_ALL=C sort output.txt -o output.txt
