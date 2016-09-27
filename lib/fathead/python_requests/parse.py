#!/usr/bin env python

from bs4 import BeautifulSoup
from glob import glob


def build_article(h2_parent):
    '''Builds fathead article entry.

    Accepts h2_parent and extracts title, url, description and code snippets.
    Returns a dict
    '''
    title = h2_parent.find('h2').text.replace('¶', '')
    print("Title %s " % title)

for html_file in glob('download/*.html'):
    print("Processing %s" % html_file)
    soup = BeautifulSoup(open(html_file), "html.parser")
    page_url = soup.find('link', attrs={'rel': 'canonical'}).get('href')
    print("Page url %s" % page_url)
    h2s = soup.findAll('h2')
    for h2 in h2s:
        build_article(h2.parent)
