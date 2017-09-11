#!/bin/bash

mkdir -p download
cd download
rm -f *.html

wget --quiet -np -nc -r https://docs.djangoproject.com/en/1.10/ref/
