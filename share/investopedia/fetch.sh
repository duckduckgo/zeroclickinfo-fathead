#!/bin/bash

mkdir -p download

url="ftp://invddg:5kzwF4GZ@ftp.mezimedia.com/TermOfTheDayForDDGForJSON_batch.xml"

wget -O ./download/terms.json $url

