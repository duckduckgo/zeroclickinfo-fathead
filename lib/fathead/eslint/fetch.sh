#!/bin/bash

# clean up
rm -fR download
mkdir -p download

# get full repo
git clone https://github.com/eslint/eslint.github.io download/repo/
