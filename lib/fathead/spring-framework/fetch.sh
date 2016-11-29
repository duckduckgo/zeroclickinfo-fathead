#!/usr/bin/env bash
# see http://stackoverflow.com/a/10959815/834
rm -rf download
rm -rf docs
mkdir -p download
mkdir -p docs/api
cd download
unamestr=`uname`
if [[ "$unamestr" == 'Darwin' ]]; then
   curl -O http://docs.spring.io/spring/docs/current/javadoc-api/allclasses-noframe.html
else
  wget --quiet --no-check-certificate --no-cookies -O http://docs.spring.io/spring/docs/current/javadoc-api/allclasses-noframe.html
fi
cd ..
python ./fetch_docs.py