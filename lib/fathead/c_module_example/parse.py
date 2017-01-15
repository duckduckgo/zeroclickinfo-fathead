#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import re
from bs4 import BeautifulSoup


BASE_URL = "http://en.cppreference.com/"
OUTPUL_FILE = open('output.txt', 'w+')
MODULE_DICT = {}
RESULT_URL = []


def print_line(title, abstract):

    # Duplicate check
    if title in MODULE_DICT:
        return

    MODULE_DICT[title] = 1

    abstract = abstract.replace('\n', '\\n')
    abstract = abstract.replace('\t', '  ')
    abstract = abstract.replace(u'\u2019', '&#8217;')
    abstract = abstract.replace(u'\u201c', '&#8220;')
    abstract = abstract.replace(u'\u201d', '&#8221;')
    abstract = abstract.replace(u'\xb6', '')
    abstract = abstract.replace(u'\xa0', '')
    abstract = abstract.replace(u'\u2013', '&#8211;')
    abstract = abstract.replace(u'\u2018', '&#8216;')

    abstract = '<section class="prog__container"><pre><code>' + abstract + \
               '</code></pre></section>'

    list_of_data = [
        title,      # 1. article title
        'A',        # 2.type is article
        '',         # 3.no redirect data
        '',         # 4.leave empty
        '',         # 5.no categories
        '',         # 6.leave empty
        '',         # 7.no related topics
        '',         # 8.leave empty
        '',    		# 9.an external link back to home
        '',         # 10.no disambiguation
        '',         # 11.images
        abstract,   # 12.abstract
        BASE_URL    # 13.url to doc
    ]

    OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))


# Fetch example code, if exists
def get_example_code(title, url):

    page = requests.get(url)
    src = page.text
    ob = BeautifulSoup(src, 'html.parser')

    print_line(title + ' ' + url.split('/')[-1], ob.find('div', {'class':
               't-example'}).text)


# Fetch the link of a particular function
def _get_func_name(url):

    page = requests.get(url)
    src = page.text
    idx = src.rfind("Functions")  # Used for getting links to only functions
    if idx == -1:
        idx = src.rfind("Member functions")  # Some are Member Functions
    if idx == -1:
        return
    src = src[idx:]
    ob = BeautifulSoup(src, 'html.parser')

    for info in ob.findAll('tr', {'class': 't-dsc'}):
        try:
            get_example_code(url.split('/')[-1], BASE_URL +
                             info.find('td').find('a')['href'])
        except:
            continue


# Extracts links to all the functions for each header file
def get_func_name():

    for url in RESULT_URL:
        _get_func_name(url)


# Extracts links to all header files from Home Page
def get_header_links():

    page = requests.get(BASE_URL)
    src = page.text
    ob = BeautifulSoup(src, 'html.parser')

    for info in ob.findAll('div', {'class': 'mainpagediv'}):
        try:
            temp = info.find('p').findAll('a')
            for i in range(len(temp)):
                RESULT_URL.append(BASE_URL + temp[i]['href'])
        except:
            continue

    get_func_name()


if __name__ == "__main__":

    get_header_links()
