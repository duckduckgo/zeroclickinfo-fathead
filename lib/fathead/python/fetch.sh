#!/bin/bash

mkdir -p download
cd download
rm -rf python-3.4.5-docs-html
rm -f *.tar.bz2
wget --quiet https://docs.python.org/3.4/archives/python-3.4.5-docs-html.tar.bz2
tar -vxjf python-3.4.5-docs-html.tar.bz2