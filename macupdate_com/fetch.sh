#!/bin/bash

# rm -rf download
mkdir -p download
# Note - just picked 1000 records arbitrarily for the moment

export MAX_PAGES=1000
export START_DATE=`date -v -6m +"%Y%m%d"`
export END_DATE=`date +"%Y%m%d"`
export MIN_DATE="2003-01"
export MAX_DATE=`date +"%Y-%m"`

curl "http://www.macupdate.com/find/?qk=&modpreset=\[$START_DATE+to+$END_DATE\]&mm=$MIN_DATE&mx=$MAX_DATE&title=&titlenot=&desc=&descnot=&dev=&devnot=&os=mac&rating=all/[0-$MAX_PAGES:80]" --output "download/macupdate_#1.html"
