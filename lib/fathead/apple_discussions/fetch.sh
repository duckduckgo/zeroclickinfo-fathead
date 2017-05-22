#!/bin/sh

mkdir -p "./download"

curl -o "./download/sitemap.xml" "https://discussions.apple.com/sitemap.jspa"
