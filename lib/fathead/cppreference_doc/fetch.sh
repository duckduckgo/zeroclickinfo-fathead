#!/bin/bash
rm -rf download
mkdir -p download

pushd download > /dev/null
wget upload.cppreference.com/w/Cppreference:DDG-link?action=raw -O cppreference-link

wget -i cppreference-link -O cppreference-doc-ddg.tar.gz
tar -xzf cppreference-doc-ddg.tar.gz
mv cppreference-doc-20* cppreference-doc
popd > /dev/null
