#!/bin/bash

mkdir -p download
cd download
rm -f *.tgz
wget http://kapeli.com/feeds/tvOS.tgz
tar -xvzf tvOS.tgz