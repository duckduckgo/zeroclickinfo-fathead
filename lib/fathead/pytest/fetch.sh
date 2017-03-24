#!/bin/bash

mkdir -p download
rm -f download/*.html
wget -O download/docs.html http://doc.pytest.org/en/latest/builtin.html --quiet