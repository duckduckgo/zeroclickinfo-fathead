#!/bin/bash

mkdir -p download
cd download
rm -f *.html
wget --quiet http://nose2.readthedocs.io/en/latest
