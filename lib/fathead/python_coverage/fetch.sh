#!/usr/bin/env bash

TO_DOWNLOAD=$(python parse.py --baseurl https://coverage.readthedocs.io/en/latest/api.html --listonly)
if [[ ! -d download ]]; then
    mkdir download
else
    rm -rf download/*
fi

cd download
for URL in ${TO_DOWNLOAD}; do
    wget --quiet ${URL}
done
