#!/bin/bash
ruby parse.rb > output.txt
LC_ALL=C sort output.txt -o output.txt
