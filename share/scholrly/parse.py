#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import namedtuple
import json, itertools, urllib, re

ABSTRACT_TEMPLATE = """
<style>
ul.scholrly.author-stats {{ 
    padding:0;
    margin-top:10px;
}}
ul.scholrly.author-stats li {{
    list-style: none;
    display: block;
    float: left;
    margin-right:25px;
    margin-bottom: 12px;
}}
ul.scholrly.author-stats li:last-child {{
    float:;
}}
</style>
<a class="snippet" href="{source_url}">
{name} is a researcher interested in {keywords}.
</a>
<ul class="scholrly author-stats">
  <li>
    {num_coauthors}
    <strong>Coauthors</strong>
  </li>
  <li>
    {num_papers}
    <strong>Papers</strong>
  </li>
  <li>
    {num_citations}
    <strong>Citations</strong>
  </li>
</ul>
<div style="clear:left;">&nbsp;</div>
"""

AUTHOR_CATEGORIES = ['researchers']

DownloadRow = namedtuple('DownloadRow',
        ['names','url','image_url','num_coauthors', 'num_papers',
         'num_citations','keywords'])

class ParsedDownloadRow(DownloadRow):
    @property
    def names(self):
        try:
            return json.loads(super(ParsedDownloadRow, self).names)
        except:
            return []

    @property
    def keywords(self):
        try:
            return json.loads(super(ParsedDownloadRow, self).keywords)
        except:
            return []

DDGOutputRow = namedtuple('DDGOutputRow',
        ['title', 'type', 'redirect', 'other_uses', 'categories', 'references',
         'see_also', 'further_reading', 'external_links', 'disambiguation',
         'images', 'abstract', 'source_url'])

def replace_whitespace(s):
    return str(s).replace('\t',' ').replace('\n', ' ').replace('\r', ' ')

WHITESPACE_PATTERN = re.compile(r'\s+')

def minify_whitespace(s):
    return WHITESPACE_PATTERN.sub(' ', s)

def ddg_search_url(query):
    return 'https://duckduckgo.com/?%s' % urllib.urlencode({'q':query})

def format_keywords(keywords):
    linked_kw = ['<a href="%s">%s</a>' % (ddg_search_url(kw), kw.lower())
                 for kw in keywords]
    first_part = ', '.join(linked_kw[:-2])
    second_part = ' and '.join(linked_kw[-2:])
    parts = [part for part in [first_part, second_part] if len(part) > 0]
    return ', '.join(parts)

def output_from_row(row):
    # generate the main page
    if len(row.names) == 0 or len(row.keywords) == 0:
        return ''
    article = DDGOutputRow(title=row.names[0],
                           type='A',
                           redirect='',
                           other_uses='',
                           categories='\\n'.join(AUTHOR_CATEGORIES),
                           references='',
                           see_also='',
                           further_reading='',
                           external_links='[%s More at Scholrly]' % row.url,
                           disambiguation='',
                           images='[[Image:%s]]' % row.image_url,
                           abstract=minify_whitespace(
                               ABSTRACT_TEMPLATE.format(
                                   name=row.names[0],
                                   source_url=row.url,
                                   num_coauthors=row.num_coauthors,
                                   num_papers=row.num_papers,
                                   num_citations=row.num_citations,
                                   keywords=format_keywords(row.keywords))),
                           source_url=row.url)
    # generate redirects for any aliases
    redirects = [DDGOutputRow(title=name, type='R',redirect=row.names[0],
                              other_uses='',categories='',references='',
                              see_also='',further_reading='',external_links='',
                              disambiguation='', images='', abstract='',
                              source_url='')
                 for name in row.names[1:]]
    return '\n'.join('\t'.join(replace_whitespace(el) for el in row)
                     for row in [article] + redirects)

used_names = set()

with open('download/download.tsv') as data_file:
    # read in the downloaded data, skipping the header
    rows = (ParsedDownloadRow(*line.split('\t'))
            for line in itertools.islice(data_file, 1, None))
    for row in rows:
        # make sure we don't use a name twice, since we don't do disambig
        # pages yet
        if all(name not in used_names and not used_names.add(name)
            for name in row.names):
            print(output_from_row(row))
