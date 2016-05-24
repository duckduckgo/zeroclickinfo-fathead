#!/bin/bash
mkdir -p download
cd download
if ! [ -d "kanjidic.xml" ] 
then 
    wget "www.csse.monash.edu.au/~jwb/kanjidic2/kanjidic2.xml.gz"
    gunzip "kanjidic2.xml.gz"
fi
cd ..
