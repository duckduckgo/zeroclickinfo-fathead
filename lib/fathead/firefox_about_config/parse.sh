#!/bin/bash

/usr/bin/env python2 parse.py
LC_ALL=C sort output.txt -o output.txt
