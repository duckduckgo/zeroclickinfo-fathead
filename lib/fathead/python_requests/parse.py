#!/usr/bin env python
# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from glob import glob
import re
try:
    from urlparse import urljoin
except ImportError:
    from urllib.parse import urljoin


def build_article(h2_parent, page_url):
    '''Builds fathead article entry.

    Accepts h2_parent and extracts title, url, description and code snippets.
    Returns a list
    '''
    h2 = h2_parent.find('h2')
    title = h2.text.replace('¶', '')
    fragment = h2_parent.find('a').get('href')
    url = urljoin(page_url, fragment)
    description = ''
    next_sibling = h2.find_next_sibling(text=None)
    while next_sibling:
        if next_sibling.name == 'p':
            next_sibling_text = next_sibling.text.replace('\n', ' \\n')
            description += " {}".format(next_sibling_text)
        elif next_sibling.name == 'div':
            pre = next_sibling.find('pre')
            if pre:
                pre_text = pre.text.replace('\n', '\\n')
                description += "<pre>{0}</pre>".format(pre_text)
        next_sibling = next_sibling.find_next_sibling(text=None)
    description = description.lstrip()
    description = description.strip('\n')
    description = "<p>{0}</p>".format(description)
    print("Title %s " % title)
    print("URL %s" % url)
    print("Description %s" % description)
    return [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            description,     # abstract
            url              # anchor to specific section
        ]

with open('output.txt', 'w') as fp:
    for html_file in glob('download/*.html'):
        print("Processing %s" % html_file)
        soup = BeautifulSoup(open(html_file), "html.parser")
        page_url = soup.find('link', attrs={'rel': 'canonical'}).get('href')
        print("Page url %s" % page_url)
        h2s = soup.findAll('h2')
        for h2 in h2s:
            data = build_article(h2.parent, page_url)
            data = '\t'.join(data)
            fp.write("{}\n".format(data))
