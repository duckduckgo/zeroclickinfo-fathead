#!/bin/bash

rm -rf download
mkdir -p download
python2.7 fetch.py
cd download
wget -nv -nc -i list_of_urls.txt
rm list_of_urls.txt
cd ..