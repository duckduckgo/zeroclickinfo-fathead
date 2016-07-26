#!/bin/bash

./node_modules/.bin/babel src --out-dir . && node parse.js
