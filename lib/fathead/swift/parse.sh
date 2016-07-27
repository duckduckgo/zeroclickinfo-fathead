#!/bin/bash

./node_modules/.bin/babel src --out-dir . && rm output.txt && node parse.js
