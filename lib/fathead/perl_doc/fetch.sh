#!/usr/bin/env bash

mkdir -p download && \
  ([[ -f perldoc-html.tar.gz ]] || wget http://perldoc.perl.org/perldoc-html.tar.gz) \
  && tar xf perldoc-html.tar.gz -C download --strip-components=1
