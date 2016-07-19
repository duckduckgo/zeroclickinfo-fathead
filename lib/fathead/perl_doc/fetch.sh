#!/usr/bin/env bash

mkdir -p download && cd download \
  && wget http://perldoc.perl.org/perldoc-html.tar.gz \
  && tar xf perldoc-html.tar.gz
