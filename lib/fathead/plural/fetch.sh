#!/bin/sh
URL=`cat data.url`
mkdir -p download
curl $URL | bunzip2 > download/wiktionary.xml
