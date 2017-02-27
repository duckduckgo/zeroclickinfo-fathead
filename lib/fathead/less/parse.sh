#!/bin/sh
node ./parse.js
LC_ALL=C sort output.txt -o output.txt
