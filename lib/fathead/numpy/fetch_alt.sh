#!/bin/bash
# Alternative documents fetcher

rm -rf numpy-docs
mkdir -p numpy-docs
cd numpy-docs

wget --no-directories --accept html --mirror --level 1 --no-parent https://docs.scipy.org/doc/numpy/reference/generated/
rm index.html

