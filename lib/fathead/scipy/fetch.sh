#!/bin/bash
VERSION=0.18.1
DOCS_ZIP=scipy-html-$VERSION.zip

rm -rf docs-tmp
wget -c https://docs.scipy.org/doc/scipy-$VERSION/$DOCS_ZIP
unzip -d docs-tmp $DOCS_ZIP

rm -rf scipy-docs
mkdir -p scipy-docs
cp docs-tmp/generated/*html scipy-docs
