#!/bin/bash

mkdir -p download
cd download
rm -rf python-3.5.2-docs-html
rm -f *.tar.bz2
wget https://docs.python.org/3.5/archives/python-3.5.2-docs-html.tar.bz2
tar -vxjf python-3.5.2-docs-html.tar.bz2

rm -rf python-2.7.12-docs-html
wget https://docs.python.org/2.7/archives/python-2.7.12-docs-html.tar.bz2
tar -vxjf python-2.7.12-docs-html.tar.bz2