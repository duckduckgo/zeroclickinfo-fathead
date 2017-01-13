#!/bin/bash

# clean up
rm -R download
mkdir -p download

# download
curl https://raw.githubusercontent.com/eslint/eslint.github.io/master/_data/rules.yml -o ./download/summary/rules.yml
