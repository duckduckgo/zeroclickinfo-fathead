#!/usr/bin env python
# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from glob import glob
import re

def build_article(tr):
    '''Builds fathead article entry.

    '''

    title = tr.td.a.contents[0]
    url = (tr.find('a')['href'])
    #Some packages don't have a description, but simply a page listing classes and interfaces (possibly enums)
    try:
        abstract = tr.find('p').contents[0]
    except:
        abstract = "More information about interfaces and classes at " + url
    title = '<span class="prog__sub">%s</span>' % title
    #Replace newline characters with a space
    abstract = re.sub('\n', ' ', abstract)
    abstract = abstract.strip()
    
    abstract = '<p>%s</p>' % abstract
    abstract = '<section class="prog__container">%s</section>' % abstract
    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    return  [  
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
            abstract,        # abstract
            url,             # anchor to specific section
        ]

with open('output.txt', 'w') as fp:
    for html_file in glob('download/*.html'):
        print('Processing %s' % html_file)
        soup = BeautifulSoup(open(html_file), 'html.parser')
        page_url = soup.find('link', attrs={'rel': 'canonical'}).get('href')
        print('Page url %s' % page_url)
        trs = soup.findAll("tr")
        for tr in trs:
            data = build_article(tr)
            print(data)
            data = '\t'.join(data)
            fp.write('{}\n'.format(data))
