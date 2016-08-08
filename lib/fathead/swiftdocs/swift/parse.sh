#!/bin/bash

rm output.txt
./node_modules/.bin/babel src --out-dir . && node parse.js
