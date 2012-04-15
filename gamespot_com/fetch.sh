#!/bin/bash
rm -rf download
rm -rf download2
rm -rf download3
rm -rf download4

mkdir -p download
mkdir -p download2
mkdir -p download3
mkdir -p download4

ruby fetch.rb

mv download/S* download2
mv download2/Sp* download4
mv download/R* download3
mv download/A* download3
