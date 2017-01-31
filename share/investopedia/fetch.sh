#!/bin/bash

mkdir -p download

url="ftp://invddg:5kzwF4GZ@ftp.mezimedia.com/TermOfTheDayForDDG.xml"

wget -O ./download/terms.json $url

