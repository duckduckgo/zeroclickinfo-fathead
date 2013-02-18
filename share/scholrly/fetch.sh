#!/bin/bash
mkdir -p download
# XXX - starting with test data
curl "https://s3.amazonaws.com/scholrly-external/ddg-test.csv" --output "download/download.tsv"
