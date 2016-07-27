#!/bin/bash -eu

url=$(<data.url)
archive_name=${url##*/}
download_dir=download

index=${download_dir}/${archive_name%_rdocs.tgz}/index.html

ruby -w parse.rb "${index}" > output.txt
