#!/bin/bash
# Alternate documentation fetcher

rm -rf scipy-docs
mkdir -p scipy-docs
cd scipy-docs

wget --no-directories --accept html --mirror --level 1 --no-parent https://docs.scipy.org/doc/scipy/reference/generated/
rm index.html

