#!/bin/bash

pushd cppreference-doc > /dev/null
python3 index2ddg.py \
    "../download/cppreference-doc/index-functions-cpp.xml" \
    "../download/cppreference-doc/reference" \
    ../output.txt --split_code_snippets

# work around a DDG bug that strips empty parentheses
# note that we can't just match '()', as that would change identifier names
sed -e 's/()\([^\t]\)/(void)\1/g' -i ../output.txt

popd > /dev/null
