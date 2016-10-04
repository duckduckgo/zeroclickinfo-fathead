#!/bin/bash

URL=$(cat data.url)
DOWNLOAD_DIR="download"
DOC_FILE="documentation.tar.gz"

if [ -d $DOWNLOAD_DIR ]
then
  rm -rf $DOWNLOAD_DIR
fi

mkdir -p $DOWNLOAD_DIR

# downloading
wget -O "$DOWNLOAD_DIR/$DOC_FILE" $URL

# extracting
cd $DOWNLOAD_DIR
tar zxf $DOC_FILE
mv php-chunked-xhtml phpdoc
rm $DOC_FILE
cd ..

exit 0
