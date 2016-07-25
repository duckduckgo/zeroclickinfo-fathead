#!/bin/bash
SECONDS=0

downloadSiteMap() {
    wget 'http://developer.mozilla.org/sitemaps/en-US/sitemap.xml' -O sitemap.xml
    # Exit if couldnt download sitemap.xml
    actualsize=$(wc -c <"sitemap.xml")
    if [ $actualsize -eq 0 ]; then
        echo "error downloading sitemap.xml from MDN"
    exit 1
    fi
}

getUrlsFromPattern() {
    parse_xml () {
        local IFS=\>
        read -d \< KEY VALUE
    }
    # Clear existing files
    mkdir -p downloads && rm -rfv downloads/*
    > .cachejournal
    # Parse xml based on patterns
    patterns=("${!1}")
    urls=(); count=0; pages=0
    while parse_xml; do
        if [[ ${KEY} == "loc" ]]; then
            url=$( (echo $VALUE | grep / | cut -d/ -f4- ) 2>&1 )
            for patt in "${patterns[@]}"
            do
                if [[ ${url} =~ ^${patt} ]]; then
                    # download pages
                    basename="${url%/*}"; filename="${basename##*/}/${url##*/}"
                    wget -P 8 -bqc -O downloads/${filename////.} ${VALUE} > /dev/null 2>&1
                    # write filename,url to cachejournal
                    echo "downloads/${filename////.},${VALUE}" >> .cachejournal
                    pages=$((pages + 1))
                fi
            done
            count=$((count + 1))
        fi
        echo -ne "Read $count : $pages pages to be downloaded"\\r
    done < sitemap.xml
    echo
}

patterns=(
    "en-US/docs/Web/API"
    "en-US/docs/Web/JavaScript/Reference/Global_Objects"
)

downloadSiteMap
getUrlsFromPattern patterns[@]

echo $SECONDS seconds
