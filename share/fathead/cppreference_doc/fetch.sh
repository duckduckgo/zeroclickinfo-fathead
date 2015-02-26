#!/bin/bash
rm -rf download
mkdir -p download

pushd download > /dev/null
wget http://upload.cppreference.com/mwiki/images/e/e7/cppreference-doc-20130729-ddg.tar.gz
tar -xzf cppreference-doc-20130729-ddg.tar.gz
popd > /dev/null
