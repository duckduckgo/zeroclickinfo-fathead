#!/bin/sh

cd $(dirname -- "$0")

./parse.py > output.txt
