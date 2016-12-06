#!/bin/bash

go run parse.go -wikiinput="download/data" -output="output.txt"
LC_ALL=C sort output.txt -o output.txt
