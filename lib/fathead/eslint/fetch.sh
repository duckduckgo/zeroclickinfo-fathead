#!/bin/bash

# clean up
rm -fR download
mkdir -p download

# summary
mkdir -p download/summary
curl https://raw.githubusercontent.com/eslint/eslint.github.io/master/_data/rules.yml -o ./download/summary/rules.yml

# full repo
git clone https://github.com/eslint/eslint.github.io download/repo/
