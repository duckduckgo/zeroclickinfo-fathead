#!/bin/sh

wget http://uscode.house.gov/download/releasepoints/us/pl/113/120/xml_uscAll@113-120.zip

mkdir download
unzip xml_uscAll@113-120.zip -d download/

rm xml_uscAll@113-120.zip
