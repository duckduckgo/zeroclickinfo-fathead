#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import requests

# Base URL for itext documentation -- list of classes are defined in this
# base URL and the individual class pages use this base
itext_docs_base_url = 'http://itextsupport.com/apidocs/itext7/7.0.1/'

# Use a shared requests session to make successive requests faster
session = requests.Session()


def gather_itext_html_files(all_classes_extension):
    """
    Getting the html file for each class in the classes list
    in the itext7 api
    """
    # Get the Base URL with the list of commands
    command_list_html = session.get(itext_docs_base_url + all_classes_extension)

    # Use BeautifulSoup to make it easy to parse
    soup = BeautifulSoup(command_list_html.text, 'html.parser')

    # Find all links
    links = soup.find_all('a')

    # Go through each link 
    for link in links:
        href = link.get('href')
        with open('download/{}.html'.format(link.text), 'wb') as outfile:
            outfile.write(bytes(session.get('{}{}'.format(itext_docs_base_url, href)).text, 'UTF-8'))

if __name__ == '__main__':
    gather_itext_html_files('allclasses-noframe.html')
