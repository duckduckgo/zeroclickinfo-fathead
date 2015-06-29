#!/bin/bash

rm -rf download
mkdir download

curl http://pkg.julialang.org/ > download/packages.html
