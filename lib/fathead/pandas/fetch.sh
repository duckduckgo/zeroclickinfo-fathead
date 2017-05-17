#!/bin/bash
VERSION=0.20.1

rm -rf docs-tmp
wget -c pandas.pydata.org/pandas-docs/version/VERSION/pandas.zip
unzip -d docs-tmp $DOCS_ZIP

rm -rf pandas-docs
mkdir -p pandas-docs
cp docs-tmp/reference/generated/*html pandas-docs
