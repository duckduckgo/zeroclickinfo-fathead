#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

import codecs
import json
import re
import urllib


def quote_url(url):
    return urllib.quote(url, safe='/:')


with codecs.open('download/package-jsons', encoding='utf-8') as in_file, \
        codecs.open('output.txt', mode='wb', encoding='utf-8') as out_file:
    for package_json in in_file:
        package_dict = json.loads(package_json)
        release_url = quote_url(package_dict['info']['release_url'])
        external_links = '[%s Release page]' % release_url
        home_page = package_dict['info'].get('home_page', None)
        if home_page and home_page.startswith('http'):
            try:
                external_links += '\\\\n[%s Official site]' % quote_url(home_page)
            except KeyError:  # Can happen for Chinese URLs -- we can live without them (only one found)
                pass

        summary = package_dict['info']['summary']
        if not summary or summary == 'UNKNOWN':
            continue
        summary = re.sub(r'\s', ' ', summary, flags=re.MULTILINE | re.UNICODE)

        out_file.write('\t'.join([
            package_dict['info']['name'],  # Title
            'A',  # Article type
            '',  # No redirect
            '',  # Other uses (ignored)
            '',  # No categories
            '',  # References (ignored)
            '',  # No related topics
            '',  # Further reading (ignored)
            external_links,
            '',  # Disambiguation (ignored)
            '',  # No images
            summary,  # Abstract
            release_url,  # Source url
        ]))
        out_file.write('\n')
