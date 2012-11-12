#!/bin/bash

# rm -rf download
mkdir -p download
# Note - just picked 30 pages arbitrarily for the moment
curl "http://pypi.python.org/pypi?%3Aaction=index" --output "download/pypy.html"

