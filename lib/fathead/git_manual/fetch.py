#!/usr/bin/env python3                                                                                                                     
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup

import requests

# Use a shared requests session to make successive requests faster
session = requests.Session()


def gather_git_command_html_files():
    ''' 
    Basic function for collecting the list of git commands that we need to get
    individual HTML files for
    '''
    # Get the Base URL with the list of commands
    command_list_html = session.get(git_docs_base_url)

    # Use BeautifulSoup to make it easy to parse
    soup = BeautifulSoup(command_list_html.text, 'html.parser')

    # Find all links
    links = soup.find_all('a')

    # Go through each link -- some are git command links, some are not
    commands = []
    for link in links:
        href = link.get('href')

        # This is how we know it's a git command page URL
        if '/docs/git-' in href:
            href = href.replace('/docs', '') 
            with open('download/{}.html'.format(link.text), 'wb') as outfile:
                outfile.write(bytes(session.get('{}{}'.format(git_docs_base_url, href)).text, 'UTF-8'))

if __name__ == '__main__':
    # Base URL for git documentation -- list of commands are defined in this base
    # URL and the individual command pages use this base URL + /git-<command_name>
    git_docs_base_url = open('data.url').read().strip()

    gather_git_command_html_files()   