"""Download pages"""

from bs4 import BeautifulSoup
import codecs
import re
import os

import requests
from tqdm import tqdm
from multiprocessing.dummy import Pool as ThreadPool

INFILE = 'download/homepage.html'
OUTFILE = 'output.txt'

HOST_URL = 'http://man7.org/linux/man-pages/'

def download_pages(url):
    response = requests.get(url)

    if response.status_code != 404:
        page_html = response.text
        
        name = url.split('/')[-1]
        destination = os.path.join('download', name)
        
        with open(destination, 'w+') as f:
            f.write(page_html)
    else:
        print('404 not found')


with codecs.open(INFILE, 'rb', encoding='utf-8') as f:
    html = BeautifulSoup(f, 'html.parser')

    pool = ThreadPool(6)

    # find all man pages
    pages = html.find_all(href=re.compile(r'^./man[0-9]/.+.html$'))
    urls = [HOST_URL + page['href'][2:] for page in pages]

    # download pages
    for _ in tqdm(pool.imap_unordered(download_pages, urls), 
                  ascii=True, desc='downloading pages:', total=len(pages)):
        pass
