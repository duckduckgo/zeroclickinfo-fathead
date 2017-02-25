#!/bin/sh

mkdir -p "./download"

wget --continue --directory-prefix="./download" --html-extension --no-parent --recursive --no-directories "http://gruntjs.com/api/grunt"

rm -rf "./download/robots.txt" "./download/exit-codes.html" "./download/inside-tasks.html" "./download/grunt.html"
