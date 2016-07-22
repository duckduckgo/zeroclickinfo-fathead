#!/bin/bash

mkdir -p download
cd download
rm -f *.html
wget --quiet http://sass-lang.com/documentation/Sass/Script/Functions.html
wget --quiet http://sass-lang.com/documentation/file.SASS_REFERENCE.html