#!/usr/bin/env bash
# see http://stackoverflow.com/a/10959815/834
rm -rf download
rm -rf docs
mkdir -p download
mkdir -p docs
cd ./download/
# This is for OSX support.  In OSX wget is not installed by default but curl is.  Uncomment it if you need to run it on OSX.
#unamestr=`uname`
#if [[ "$unamestr" == 'Darwin' ]]; then
#   curl -O http://docs.spring.io/spring/docs/current/javadoc-api/overview-summary.html
#else
  wget --quiet --no-check-certificate --no-cookies -O overview-summary.html http://docs.spring.io/spring/docs/current/javadoc-api/overview-summary.html
#fi
cd ..
python ./fetch.py
unzip -q ./download/*.zip -d ./docs/
