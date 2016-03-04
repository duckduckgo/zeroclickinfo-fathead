#!/bin/bash
python fetch.py --cachedir downloads \
                --cachejournal ".cachejournal_api" \
                --patt "en-US/docs/Web/API" \
                --sitemap http://developer.mozilla.org/sitemaps/en-US/sitemap.xml \
                --sleep 5

python fetch.py --cachedir downloads \
                --cachejournal ".cachejournal_js" \
                --patt "en-US/docs/Web/JavaScript/Reference/Global_Objects" \
                --sitemap http://developer.mozilla.org/sitemaps/en-US/sitemap.xml \
                --sleep 5

cat .cachejournal_api >> .cachejournal
cat .cachejournal_js >> .cachejournal
