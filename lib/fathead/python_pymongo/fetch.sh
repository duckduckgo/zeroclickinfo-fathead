#!/usr/bin/env bash

URL="http://api.mongodb.com/python/current/api/index.html"
if [[ ! -d download ]]; then
    mkdir download
else
    rm -rf download/*
fi

cd download
wget --quiet -r --no-parent -nH -m --cut-dirs=3 ${URL}
