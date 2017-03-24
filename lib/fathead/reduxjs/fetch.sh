#!/bin/sh

mkdir -p "./download"

 wget --continue --directory-prefix="./download" --html-extension --no-parent --recursive --no-directories "redux.js.org/docs/api/"

rm -rf "./download/index.html"
