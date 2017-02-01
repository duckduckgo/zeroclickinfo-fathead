#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import sys
import re
from bs4 import BeautifulSoup

reload(sys)
sys.setdefaultencoding('utf8')

START_URL = 'http://electron.atom.io/docs/api/'
OUTPUL_FILE = open('output.txt', 'w+')
RESULT_URLS = []


# Print the content to the output file
def print_article_line(title, content, url):

    _platform = ''
    content = content.replace('\n', '\\n').replace('\t', '  ')
    if 'Windows' in title:
        _platform = 'Windows'
    if 'macOS' in title:
        _platform = _platform + ' macOS'
    if _platform:
        content = '<span class="prog__sub>Platform:</span><p>'+_platform+'</p>'
    title = title.replace('Windows', '').replace('macOS', '')
    abstract = '<section class="prog__container">' + content + '</section>'
    abstract = ''.join([i if ord(i) < 128 else ' ' for i in abstract])

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
        url   # 13.url to doc
    ]

    OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))


# Extracts content of all the pages
def get_func_desc():

    for url in RESULT_URLS:
        title = url.split('/')[-1].lower().replace('-', ' ')
        content = ""

        page = requests.get(url)
        src = page.text

        idx = src.find('<h2')
        main_desc = src[:idx]  # To extract the main content from a page
        ob_main = BeautifulSoup(main_desc, 'html.parser')

        info = ob_main.find('blockquote')

        while info:
            if info.name == 'p':
                content = content + re.sub(r'\<a.*\"\>|\<\/a\>', '', str(info))
            elif info.name == 'ul':
                content = content + re.sub(r'\<a.*\"\>|\<\/a\>', '', str(info))
                .replace('<ul>', '<ul class="prog__ul">')
            elif info.name == 'h3':
                content = content + '<span class=prog__sub">' +
                info.text + '</span>'
            elif info.name == 'div':
                if 'highlighter-rouge' in info['class']:
                    content = content + '<pre><code>' + \
                              info.text + '</code></pre>'
            info = info.find_next()

        print_article_line(title, content, url)  # Print content to the file

        # Look for methods and class in the page
        idx = src.find('Methods')
        method_desc = src[idx:]
        ob_method = BeautifulSoup(method_desc, 'html.parser')

        info = ob_method.find('p')
        content = ""
        first_time = 0

        while info:
            if info.name == 'h3' or info.name == 'h4':
                if first_time == 0:
                    first_time = 1
                else:
                    if title[:8] != 'Instance':
                        title = re.sub(r'\([^)]*\)', '', title)
                        if 'Event' in title:
                            title = title.replace('Event: ', '')
                            .replace('‘', '')
                            title = url.split('/')[-1].lower()
                            .replace('-', ' ') + ' ' + title
                        print_article_line(title, content, url)
                    content = ""
                title = info.text
            elif info.name == 'p' and first_time == 1:
                content = content + re.sub(r'\<a.*\"\>|\<\/a\>', '', str(info))
            elif info.name == 'ul' and first_time == 1:
                content = content + re.sub(r'\<a.*\"\>|\<\/a\>', '', str(info))
                .replace('<ul>', '<ul class="prog__ul">')
            elif info.name == 'div' and first_time == 1:
                if 'highlighter-rouge' in info['class']:
                    content = content + '<pre><code>' + \
                              info.text + '</code></pre>'
            info = info.find_next()


# Extracts links to all pages from Home Page
def get_page_links():

    page = requests.get(START_URL)
    src = page.text
    ob = BeautifulSoup(src, 'html.parser')

    info = ob.find('table', {'class': 'table-with-spacious-second-column'})
    info = info.findAll('td')
    for i in info:
        try:
            RESULT_URLS.append(START_URL + i.find('a')['href'].split('/')[-2])
        except:
            continue

    get_func_desc()


if __name__ == '__main__':

    get_page_links()
