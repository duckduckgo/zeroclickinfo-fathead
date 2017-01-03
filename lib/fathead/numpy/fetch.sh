#!/bin/bash
DOCS_ZIP=numpy-html-1.11.0.zip

rm -rf docs-tmp
wget -c https://docs.scipy.org/doc/numpy/$DOCS_ZIP
unzip -d docs-tmp $DOCS_ZIP

rm -rf numpy-docs
mkdir -p numpy-docs
cp docs-tmp/reference/generated/*html numpy-docs
