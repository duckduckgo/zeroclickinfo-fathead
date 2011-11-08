#!/usr/local/bin/bash

# rm -rf download
mkdir -p download
# Note - just picked 30 pages arbitrarily for the moment
curl "http://{osx,iphone,windows}.iusethis.com/?page=[1-30]" --output "download/iusethis_#1_#2.html"

