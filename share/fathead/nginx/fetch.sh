#!/bin/bash
set -e

DOWNLOAD_DIR="download"

error() {
    echo "Error:" "$@" >&2
    exit 1
}

# To get the needed data, we could parse the html files on the nginx
# site. However, the html files for sure change once in a while and
# each change might break the parsing code. Hence, we directly use the
# documentation's xml sources instead.
#
# To get all needed xml documentation sources we could wget/curl the files
# from
#   http://hg.nginx.org/nginx.org/raw-file/default/xml/en/docs/http
# but just cloning the tiny repo is twice as fast. Hence, we clone the
# repo of the Nginx.org site.

if [ -e "$DOWNLOAD_DIR" ]
then
    if [ -d "$DOWNLOAD_DIR/.hg" ]
    then
        rm -rf "$DOWNLOAD_DIR"
    else
        error "'$DOWNLOAD_DIR' exists, but it's not a hg clone. Aborting."
    fi
fi

hg clone http://hg.nginx.org/nginx.org "$DOWNLOAD_DIR"

echo "Fetching done."