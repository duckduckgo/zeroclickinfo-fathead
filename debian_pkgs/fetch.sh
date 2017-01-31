#!/bin/bash
mkdir -p download

wget -q -N  'http://packages.debian.org/unstable/allpackages?format=txt.gz' -O 'download/unstable.txt.gz'
wget -q -N  'http://packages.debian.org/stable/allpackages?format=txt.gz' -O 'download/stable.txt.gz'
wget -q -N  'http://packages.debian.org/testing/allpackages?format=txt.gz' -O 'download/testing.txt.gz'

