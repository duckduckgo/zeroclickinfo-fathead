#!/usr/bin env python

from bs4 import BeautifulSoup
from glob import glob

for html_file in glob('download/*.html'):
    print("Processing %s" % html_file)
    soup = BeautifulSoup(open(html_file))
    page_url = soup.find('link', attrs={'rel': 'canonical'}).get('href')
    print("Page url %s" % page_url)
