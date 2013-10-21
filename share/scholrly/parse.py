#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import namedtuple
import json, itertools, urllib, re, sys

ABSTRACT_TEMPLATE = unicode("""
{name} is a researcher{keyword_phrase}. {last_name} has written {num_papers} paper{paper_prefix} with {num_coauthors} coauthor{coauthor_prefix} and {num_citations} citation{citation_prefix}.
""")

AUTHOR_CATEGORIES = ['researchers']

DownloadRow = namedtuple('DownloadRow',
        ['names','url','image_url','num_papers', 'num_coauthors',
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
    return unicode(s).replace('\t',' ').replace('\n', ' ').replace('\r', ' ')

WHITESPACE_PATTERN = re.compile(r'\s+')

def minify_whitespace(s):
    return WHITESPACE_PATTERN.sub(' ', s)

def ddg_search_url(query):
    return 'https://duckduckgo.com/?%s' % urllib.urlencode({'q':query})

def format_keywords(keywords):
    linked_kw = [kw.lower() for kw in keywords]
    first_part = ', '.join(linked_kw[:-2])
    second_part = ' and '.join(linked_kw[-2:])
    parts = [part for part in [first_part, second_part] if len(part) > 0]
    return ', '.join(parts)

def output_from_row(row):
    # generate the main page
    if len(row.names) == 0 or len(row.keywords) == 0:
        return ''
    
    # NB these templating funcs expect n >= 0
    def number_or_no(n):
        return unicode(n) if n > 0 else 'no'

    def plural_suffix(n):
        return 's' if n > 1 or n == 0 else ''

    keyword_phrase = ' interested in %s' % format_keywords(row.keywords) \
            if len(row.keywords) > 0 else ''

    # NB this is not the best way to handle last names (at all), but should
    # work for the majority of cases right now
    last_name = row.names[0].split()[-1]

    num_coauthors = number_or_no(row.num_coauthors)
    coauthor_prefix = plural_suffix(row.num_coauthors)

    num_papers = number_or_no(row.num_papers)
    paper_prefix = plural_suffix(row.num_papers)

    num_citations = number_or_no(row.num_citations)
    citation_prefix = plural_suffix(row.num_citations)

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
                                   last_name=last_name,
                                   num_coauthors=num_coauthors,
                                   coauthor_prefix=coauthor_prefix,
                                   num_papers=num_papers,
                                   paper_prefix=paper_prefix,
                                   num_citations=num_citations,
                                   citation_prefix=citation_prefix,
                                   keyword_phrase=keyword_phrase)),
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

if __name__ == '__main__':
    with open(sys.argv[1]) as data_file:
        # read in the downloaded data, skipping the header
        rows = (ParsedDownloadRow(*line.split('\t'))
                for line in itertools.islice(data_file, 1, None))
        with open(sys.argv[2], 'a') as output_file:
            for row in rows:
                # make sure we don't use a name twice, since we don't do disambig
                # pages yet
                if all(name not in used_names and not used_names.add(name)
                    for name in row.names):
                    output_file.write(output_from_row(row).encode('utf8') + '\n')
