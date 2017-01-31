#!/bin/bash
mkdir -p download
# XXX - starting with test data
curl "https://s3.amazonaws.com/scholrly-external/ddg-2013-4-3.tsv" --output "download/download.tsv"
