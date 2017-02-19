#!/bin/bash

# clean up
rm -R download
mkdir -p download

# summary
mkdir -p download/summary
curl https://raw.githubusercontent.com/eslint/eslint.github.io/master/_data/rules.yml -o ./download/summary/rules.yml

# full repo
## curl https://github.com/eslint/eslint.github.io/archive/master.zip -o ./download/repo.zip
## unzip ./download/repo.zip -d ./download/repo/
git clone https://github.com/eslint/eslint.github.io download/repo/
