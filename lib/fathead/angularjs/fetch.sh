#!/bin/bash

mkdir -p download
wget -O ./download/search-data.json https://docs.angularjs.org/js/search-data.json
DATAURL=$(curl https://docs.angularjs.org/js/search-data.json | grep \"path\": | sed -e s/^[[:space:]]*\"path\"\:[[:space:]]\"// | sed -e s/\",$//)
ANGULARAPI='https://docs.angularjs.org/partials/'
for url in $DATAURL
do
    curl $ANGULARAPI$url.html --create-dirs -o ./download/$url.html
done

chmod 755 -R download/