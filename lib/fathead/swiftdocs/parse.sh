#!/bin/bash

rm output.txt
./node_modules/.bin/babel src --out-dir . && node parse.js
LC_ALL=C sort output.txt -o output.txt
