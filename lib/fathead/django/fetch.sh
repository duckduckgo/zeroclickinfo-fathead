#!/bin/bash

mkdir -p download;
cd download;
rm -f *.html;
wget --quiet https://docs.djangoproject.com/en/1.9/ref/templates/builtins/