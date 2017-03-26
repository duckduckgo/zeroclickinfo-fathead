#!/bin/bash

# we can use glob since the archive contains a single directory
pushd download/*/ > /dev/null
python3 index2ddg.py index-functions-cpp.xml ../../output.txt --split_code_snippets

# work around a DDG bug that strips empty parentheses
# note that we can't just match '()', as that would change identifier names
sed -e 's/()\([^\t]\)/(void)\1/g' -i ../../output.txt

popd > /dev/null
