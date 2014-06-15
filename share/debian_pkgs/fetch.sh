#!/bin/sh

mkdir download
wget https://packages.debian.org/stable/allpackages?format=txt.gz -O download/stable.txt.gz
wget https://packages.debian.org/testing/allpackages?format=txt.gz -O download/testing.txt.gz

gunzip download/stable.txt.gz
gunzip download/testing.txt.gz
