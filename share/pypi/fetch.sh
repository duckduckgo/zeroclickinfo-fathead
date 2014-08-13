#!/bin/bash

rm -rf download
mkdir -p download
curl -L "https://pypi.python.org/pypi?%3Aaction=index" --output "download/pypy.html"

