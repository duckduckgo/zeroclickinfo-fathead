#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import re
from bs4 import BeautifulSoup


START_URL = 'https://docs.npmjs.com/cli/install'
BASE_URL = 'https://docs.npmjs.com'
OUTPUL_FILE = open('output.txt', 'w+')
RESULT_URLS = []


# Print the content to the output file
def print_article_line(title, code, content):

    title = 'npm-' + title
    code = code.replace('\n', '\\n').replace('\t', '  ')
    code = code.replace('<', '&lt;').replace('>', '&gt;')
    content = content.replace('\n', '\\n').replace('\t', '  ')
    content = content.replace('<', '&lt;').replace('>', '&gt;')

    abstract = '<section class="prog__container">' + content + \
               '<code><pre>' + code + '</pre></code></section>'

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


# Print the redirect info to the output file
def print_redirect_line(title, code):

    # Redirect
    new_title = 'npm-' + title
    command_dict = {}

    # Always add the original title
    list_of_data = [
        title,            # 1. article title
        'R',              # 2.type is article
        new_title,        # 3.no redirect data
        '',               # 4.leave empty
        '',               # 5.no categories
        '',               # 6.leave empty
        '',               # 7.no related topics
        '',               # 8.leave empty
        '',               # 9.an external link back to home
        '',               # 10.no disambiguation
        '',               # 11.images
        '',               # 12.abstract
        ''                # 13.url to doc
        ]

    OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))

    for i in code.split('\n'):
        # Check explicitly for alias and aliases, both follow diff pattern
        if i.startswith('aliases'):
            temp = str(i.split('aliases: ')[-1])
            temp = temp.split(', ')

            for alias in temp:
                list_of_data = [
                    alias,            # 1. article title
                    'R',              # 2.type is article
                    new_title,        # 3.no redirect data
                    '',               # 4.leave empty
                    '',               # 5.no categories
                    '',               # 6.leave empty
                    '',               # 7.no related topics
                    '',               # 8.leave empty
                    '',               # 9.an external link back to home
                    '',               # 10.no disambiguation
                    '',               # 11.images
                    '',               # 12.abstract
                    ''                # 13.url to doc
                    ]

                OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))

            continue

        if i.startswith('alias'):
            command = i.split('npm ')[-1]

            list_of_data = [
                command,          # 1. article title
                'R',              # 2.type is article
                new_title,        # 3.no redirect data
                '',               # 4.leave empty
                '',               # 5.no categories
                '',               # 6.leave empty
                '',               # 7.no related topics
                '',               # 8.leave empty
                '',               # 9.an external link back to home
                '',               # 10.no disambiguation
                '',               # 11.images
                '',               # 12.abstract
                ''                # 13.url to doc
            ]

            OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))

            continue

        # Other irrelevant info
        if not i or not i.startswith('npm'):
            continue

        command = re.split(r' [<[()]', i)[0].replace('npm ', '')
        temp = re.findall(r'[^a-z](-+[a-z]+)', i)

        if not temp and not command == title:
            command_dict[command] = True

            list_of_data = [
                command,          # 1. article title
                'R',              # 2.type is article
                new_title,        # 3.no redirect data
                '',               # 4.leave empty
                '',               # 5.no categories
                '',               # 6.leave empty
                '',               # 7.no related topics
                '',               # 8.leave empty
                '',               # 9.an external link back to home
                '',               # 10.no disambiguation
                '',               # 11.images
                '',               # 12.abstract
                ''                # 13.url to doc
            ]

            OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))

            continue

        for option in temp:
            final_command = command + ' ' + option

            if final_command in command_dict:
                continue

            command_dict[final_command] = True

            list_of_data = [
                final_command,    # 1. article title
                'R',              # 2.type is article
                new_title,        # 3.no redirect data
                '',               # 4.leave empty
                '',               # 5.no categories
                '',               # 6.leave empty
                '',               # 7.no related topics
                '',               # 8.leave empty
                '',               # 9.an external link back to home
                '',               # 10.no disambiguation
                '',               # 11.images
                '',               # 12.abstract
                ''                # 13.url to doc
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

        print_article_line(title, code, content)
        print_redirect_line(title, code)


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


if __name__ == '__main__':

    get_page_links()
