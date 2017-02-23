#!/bin/bash

mkdir downloads
rm -f downloads/*.html 
wget -O download/docs.html http://doc.pytest.org/en/latest/builtin.html --quiet
