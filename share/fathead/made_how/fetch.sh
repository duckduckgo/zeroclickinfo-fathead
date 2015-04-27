#!/bin/bash

mkdir -p download

x=0
for (( ; ; )); do
    ((x+=1))
    curl -s "http://www.madehow.com/Volume-$x/index.html" -o "download/Volume-$x"
    grep -q "<title>Volume $x</title>" "download/Volume-$x"
    if [[ $? -ne 0 ]]; then
	rm "download/Volume-$x"
	break
    fi
done
