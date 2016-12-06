#!/bin/bash

cd $(dirname -- "$0")
# We do nothing here because the fetch.sh is actually grabbing something in the correct format.

LC_ALL=C sort output.txt -o output.txt
