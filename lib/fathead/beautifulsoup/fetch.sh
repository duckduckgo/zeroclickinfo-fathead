#!/bin/bash

mkdir -p download && cd download
rm -f *.html
wget --quiet https://www.crummy.com/software/BeautifulSoup/bs4/doc/
