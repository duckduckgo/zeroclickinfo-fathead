#!/bin/bash

URL=$(cat data.url)
DOWNLOAD_DIR="download"
DOC_FILE="gradle_doc_overview.html"

if [ -d $DOWNLOAD_DIR ]
then
  rm -rf $DOWNLOAD_DIR
fi

mkdir -p $DOWNLOAD_DIR

# downloading
curl $URL -o "$DOWNLOAD_DIR/$DOC_FILE"

exit 0
