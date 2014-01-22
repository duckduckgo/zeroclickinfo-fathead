#!/bin/bash
python fetch.py --cachedir downloads \
                --cachejournal ".cachejournal" \
                --patt "en-US/docs/Web/JavaScript/Reference/Global_Objects" \
                --sitemap http://developer.mozilla.org/sitemaps/en-US/sitemap.xml \
                --sleep 5
