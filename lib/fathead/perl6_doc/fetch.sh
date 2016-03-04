#!/bin/bash
# Alternatively, it could download https://github.com/perl6/doc repo, but
# generating files from it would require having Perl 6 (not yet ready for
# production usage).
mkdir -p download
cd download
wget -np -nc -r -l 2 http://doc.perl6.org/
cd ..
