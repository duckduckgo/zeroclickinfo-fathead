#!/bin/bash

mkdir -p download
rm -f download/*.html
wget -O download/docs.html https://arrow.readthedocs.io/en/latest --quiet