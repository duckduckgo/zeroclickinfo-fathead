#!/bin/bash

# set this to true if you only wish to test the scripts, to save time and server bandwith
USE_STATIC_DUMP=false

cd $(dirname -- "$0")

if $USE_STATIC_DUMP
then
  wget -nc 'ftp://ks384172.kimsufi.com/lkddb-dump-2011-10-30.7z'
  7z x lkddb-dump-2011-10-30.7z 
else
  wget -P download -nv -r -np -nc -l 2 -w 0.1 --random-wait 'http://cateee.net/lkddb/web-lkddb/index.html'
fi

