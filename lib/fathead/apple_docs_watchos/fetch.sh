#!/bin/bash

mkdir -p download
cd download
rm -f *.tgz
wget http://kapeli.com/feeds/watchOS.tgz
tar -xvzf watchOS.tgz