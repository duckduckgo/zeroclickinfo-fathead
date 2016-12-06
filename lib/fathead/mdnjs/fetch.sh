#!/bin/bash
let SECONDS=COUNT=PAGES=0

downloadSiteMap() {
    wget 'http://developer.mozilla.org/sitemaps/en-US/sitemap.xml' -O sitemap.xml
    # Exit if couldnt download sitemap.xml
    if [ $? -gt 0 ] || [ $(wc -c <"sitemap.xml") -eq 0 ] ; then
        echo "error downloading sitemap.xml from MDN"
        exit 1
    fi
}

getUrlsFromPattern() {
    # Clear existing files
    mkdir -p downloads && rm -rfv downloads/* > /dev/null 2>&1
    :> .cachejournal
    # Parse xml based on patterns
    patterns=("${!1}")
    while IFS=\> read -d \< KEY VALUE; do
        if [[ ${KEY} == "loc" ]]; then
            url=$( (grep / <<< "$VALUE" | cut -d/ -f4- ))
            for patt in "${patterns[@]}"
            do
                if [[ ${url} =~ ^${patt} ]]; then
                    # download files
                    basename="${url%/*}"; filename="${basename##*/}/${url##*/}"
                    wget -bc -O "downloads/${filename////.}" ${VALUE} -nv -o wget.log > /dev/null 2>&1
                    # write filename,url to cachejournal
                    echo "downloads/${filename////.},${VALUE}" >> .cachejournal
                    PAGES=$((PAGES + 1))
                    # pause every 50 pages
                    if [ $(( $PAGES % 50 )) -eq 0 ] ; then
                        wait; sleep 2
                    fi
                fi
            done
            COUNT=$((COUNT + 1))
        fi
        echo -ne "Read $COUNT : $PAGES pages"\\r
    done < sitemap.xml
    echo
}

patterns=(
    "en-US/docs/Web/API"
    "en-US/docs/Web/JavaScript/Reference/Global_Objects"
    "en-US/docs/Web/JavaScript/Reference/Errors"
    "en-US/docs/Web/JavaScript/Reference/Functions"
    "en-US/docs/Web/JavaScript/Reference/Classes"
    "en-US/docs/Web/JavaScript/Reference/Statements"
    "en-US/docs/Web/JavaScript/Reference/Operators"
)

downloadSiteMap
getUrlsFromPattern patterns[@]

echo " $(ls downloads | wc -l) files downloaded"
echo "$SECONDS seconds"
echo -e "Check wget.log for errors, re-run fetch to fix parse incomplete file failures."

# @mbad0la NOV 2016
# Test run : Read 15802 : 4372 pages
#            383 seconds
