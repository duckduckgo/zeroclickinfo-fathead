#!/bin/bash
set -e
set -o pipefail

XSL_FILE="extract_directives.xsl"
XML_DIR="download/xml/en/docs/http"

TARGET_FILE="output.txt"

TMP_TARGET_FILE="$TARGET_FILE"

rm -f "$TARGET_FILE"

convert_xml_to_tsv() {
    for XML_FILE in "$XML_DIR"/ngx_http_*_module.xml
    do
        xsltproc "$XSL_FILE" "$XML_FILE"
    done
}

disambiguate() {
    perl disambiguate.pl
}

convert_xml_to_tsv | sort | disambiguate >"$TMP_TARGET_FILE"

echo "Parsing done."