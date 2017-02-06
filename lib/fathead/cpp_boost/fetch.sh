#!/bin/bash

mkdir -p download
cd download
rm -f *.html
wget http://www.boost.org/doc/libs/1_63_0 -O index.html
