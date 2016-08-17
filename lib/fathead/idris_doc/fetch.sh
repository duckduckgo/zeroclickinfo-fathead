#!/usr/bin/env sh
wget -r -np -nH -R 'index.html*' http://www.idris-lang.org/docs/current/ -P download
