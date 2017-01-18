#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import re
from bs4 import BeautifulSoup


START_URL = "https://docs.npmjs.com/cli/access"
BASE_URL = "https://docs.npmjs.com"
OUTPUL_FILE = open('output.txt', 'w+')
RESULT_URLS = []


# Print the content to the output file
def print_line(title, code, content):

    code = code.replace('\n', '\\n').replace('\t', '  ')
    code = code.replace('<', '&lt;').replace('>', '&gt;')
    content = content.replace('\n', '\\n').replace('\t', '  ')
    content = content.replace('<', '&lt;').replace('>', '&gt;')

    abstract = '<section class="prog__container"><pre><code>' + code + \
               '</code></pre>' + content + '</section>'

    list_of_data = [
        title,      # 1. article title
        'A',        # 2.type is article
        '',         # 3.no redirect data
        '',         # 4.leave empty
        '',         # 5.no categories
        '',         # 6.leave empty
        '',         # 7.no related topics
        '',         # 8.leave empty
        '',         # 9.an external link back to home
        '',         # 10.no disambiguation
        '',         # 11.images
        abstract,   # 12.abstract
        START_URL   # 13.url to doc
    ]

    OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))


# Extracts content of all the pages
def get_func_desc():

    for url in RESULT_URLS:
        title = url.split('/')[-1]
        page = requests.get(url)
        src = page.text
        ob = BeautifulSoup(src, 'html.parser')

        info = ob.find('code')
        code = info.text

        info = ob.findAll('p')
        content = info[0].text

        print_line(title, code, content)


# Extracts links to all pages from Home Page
def get_page_links():

    page = requests.get(START_URL)
    src = page.text
    ob = BeautifulSoup(src, 'html.parser')

    info = ob.find('section', {'class': 'active'})
    info = info.find('div', {'class': 'pages'})
    info = info.findAll('a')

    for i in info:
        RESULT_URLS.append(BASE_URL+i['href'])

    get_func_desc()


if __name__ == "__main__":

    get_page_links()
