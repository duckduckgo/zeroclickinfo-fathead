#!/bin/bash

mkdir -p download
cd download
rm -rf python-3.5.2-docs-html
rm -f *.tar.bz2
wget https://docs.python.org/3.5/archives/python-3.5.2-docs-html.tar.bz2
tar -vxjf python-3.5.2-docs-html.tar.bz2