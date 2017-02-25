#!/bin/bash

mkdir -p download && cd download
rm -f *.html
wget --quiet https://redis-py.readthedocs.io/en/latest/
