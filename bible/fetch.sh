#!/bin/bash

cd $(dirname -- "$0")

wget -P download -N 'http://downloads.believersresource.com/bibles/rawdata/sql/sqlbible.zip'

unzip -o download/sqlbible.zip -d download

