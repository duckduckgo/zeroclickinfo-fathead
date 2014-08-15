#!/bin/bash

rm -rf download
mkdir -p download/package-jsons
cd download
wget -nv -O simple-index.html https://pypi.python.org/simple/

# Extract the package names from the simple index file.
grep -oP "<a href.*?>" simple-index.html | sed -e "s/<a href='//" -e "s/'>//" > package-names.txt

# Download all the package JSONs using 10 parallel processes.
# Inspired by http://eigenjoy.com/2010/09/06/a-crawler-using-wget-and-xargs/ (can probably be made faster by using
# lightweight threads in Python or equivalents in other languages, but then it won't be a one-liner)
cat package-names.txt | xargs -P 10 -I _PACKAGE_NAME_ wget -nv -O package-jsons/_PACKAGE_NAME_ https://pypi.python.org/pypi/_PACKAGE_NAME_/json
