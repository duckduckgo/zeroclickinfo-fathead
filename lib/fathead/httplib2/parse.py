#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

import re
from bs4 import BeautifulSoup

doc_file = "download/libhttplib2.html"
external_link = "https://httplib2.readthedocs.io/en/latest/libhttplib2.html"


# Makes the abstract formatted from a definition line, returns a tuple
# with the abstract and the source url
def parse_dl(dl):
    # extract the link before the signature to crop the pilcrow (¶)
    link = dl.dt.find("a", {'class': 'headerlink'}).extract()
    link = '{}{}'.format(external_link, link['href'])

    flags = re.MULTILINE | re.UNICODE

    signature = dl.dt.text
    signature = re.sub(r'\s', ' ', signature, flags=flags).strip()
    signature = '<pre><code>{}</code></pre>'.format(signature)

    description = dl.dd.p.text
    description = re.sub(r'\s', ' ', description, flags=flags).strip()
    description = '<p>{}</p>'.format(description)

    abstract = '<div class="prog__container">{}{}</div>'.format(signature,
                                                                description)

    return (abstract, link)


with open(doc_file, 'r') as doc:
    html = doc.read()

soup_data = BeautifulSoup(html, 'html.parser')
# remove unrelevant descriptions
for dl in soup_data.find_all('dl', {'class': 'describe'}):
    dl.extract()
# remove version information
soup_data.find('div', {'class': 'rst-versions'}).extract()

# All the definition lines from the documentation page
dls = soup_data.find_all('dl')

lines = []
for dl in dls:
    abstract, url = parse_dl(dl)
    line = [
        dl.dt['id'],    # full article title
        'A',            # type of article
        '',             # not redirect, ignored
        '',             # ignore
        '',             # no category
        '',             # ignore
        '',             # related topics
        '',             # ignore
        external_link,  # external links
        '',             # not disambiguation, ignored
        '',             # no image
        abstract,       # abstract
        url             # url
    ]
    lines.append(u'\t'.join(line))


print('\n'.join(lines))
