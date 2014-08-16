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
        package_info = package_dict['info']

        # Extract external links
        release_url = quote_url(package_info['release_url'])
        external_links = '[%s Release page]' % release_url
        home_page = package_info.get('home_page', None)
        if home_page and home_page.startswith('http'):
            try:
                external_links += '\\\\n[%s Official site]' % quote_url(home_page)
            except KeyError:  # Can happen for Chinese URLs -- we can live without them (only one found)
                pass

        # Build abstract
        abstract_lines = []
        summary = package_info['summary']
        if not summary or summary == 'UNKNOWN':
            continue
        abstract_lines.append(re.sub(r'\s', ' ', summary, flags=re.MULTILINE | re.UNICODE))
        abstract_lines.append('Downloads in the last month: %s' % package_info['downloads']['last_month'])
        latest_release_info = package_dict['releases'][package_info['version']]
        if latest_release_info:
            abstract_lines.append('Release date: %s' % latest_release_info[0]['upload_time'].split('T')[0])
        for classifier in package_info['classifiers']:
            if classifier.startswith('Development Status'):
                abstract_lines.append('Development status: %s' % classifier.split(' - ')[-1])
                break

        out_file.write('\t'.join([
            package_info['name'],  # Title
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
            '<br>'.join(abstract_lines),
            release_url,  # Source url
        ]))
        out_file.write('\n')
