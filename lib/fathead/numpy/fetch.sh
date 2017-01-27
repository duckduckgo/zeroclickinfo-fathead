#!/bin/bash
VERSION=1.12.0
DOCS_ZIP=numpy-html-$VERSION.zip

rm -rf docs-tmp
wget -c https://docs.scipy.org/doc/numpy-$VERSION/$DOCS_ZIP
unzip -d docs-tmp $DOCS_ZIP

rm -rf numpy-docs
mkdir -p numpy-docs
cp docs-tmp/reference/generated/*html numpy-docs
