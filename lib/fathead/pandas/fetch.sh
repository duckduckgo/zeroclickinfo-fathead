#!/bin/bash

rm -rf docs-tmp
wget -c pandas.pydata.org/pandas-docs/stable/pandas.zip
unzip -d docs-tmp pandas.zip

rm -rf pandas-docs
mkdir -p pandas-docs
cp docs-tmp/html/generated/*html pandas-docs
