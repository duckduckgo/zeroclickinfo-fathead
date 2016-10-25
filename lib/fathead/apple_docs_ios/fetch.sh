#!/bin/bash

mkdir -p download
cd download
rm -f *.tgz
wget http://kapeli.com/feeds/iOS.tgz
tar -xvzf iOS.tgz