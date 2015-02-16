#!/bin/sh

curl http://api.worldbank.org/v2/en/indicator/ny.gdp.mktp.cd?downloadformat=csv -o ./download/gdp.zip
unzip ./download/gdp.zip -d ./download
tail -n +3 ./download/ny.gdp.mktp.cd_Indicator_en_csv_v2.csv > ./download/gdp.csv