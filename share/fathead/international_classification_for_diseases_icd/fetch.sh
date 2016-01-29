#!/bin/sh

wget -P download -N 'https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip'

unzip -o download/2016-Code-Descriptions-in-Tabular-Order.zip -d download

wget -P download -N 'www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip';

unzip -o download/ICD10CM_FY2016_code_descriptions.zip -d download