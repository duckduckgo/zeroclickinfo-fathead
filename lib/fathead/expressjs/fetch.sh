#!/bin/bash

mkdir -p downloads
rm -f downloads/*.html 
wget -O downloads/docs.html http://expressjs.com/en/api.html --quiet
