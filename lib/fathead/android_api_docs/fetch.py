#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup

import requests
import os.path

# Shared requests session for quicker pulls
session = requests.Session()

index = 'https://developer.android.com/reference/packages.html'
baseurl = 'https://developer.android.com/'

#Layer 1, more to be made for the drill downs
def layer_1_pull(url_data):
    '''
    Downloads the html source files listed on the first packages index page
    '''
    # BeautifulSoup the page data for searching
    soup = BeautifulSoup(url_data.text, 'html.parser')

    # Locate all URLs from the table at layer 1
    trs = soup.findAll("tr")

    for tr in trs:
        # Wrap url pull in a try/except in case there is a tr without an href in it
        try:
            #title = tr.td.a.contents[0]
            url = (tr.find('a')['href'])
            print("Layer 1 URL Pull:" + url)
            # Get the folder path of the doc for saving and later parsing
            if 'https://developer.android.com/' in url:
                urlPath= url.replace('https://developer.android.com/', '')

                # Save this page in its relative local folder
                # Credit to Krumelur on stackoverflow.com for the path existence check
                filename = "download/" + urlPath
                if not os.path.exists(os.path.dirname(filename)):
                    try:
                        os.makedirs(os.path.dirname(filename))
                    except OSError as exc: # Guard against race condition
                        if exc.errno != errno.EEXIST:
                            raise

                r = session.get(url)
                with open(filename, 'wb') as outfile:
                    outfile.write(r.text.encode('utf-8'))
        except:
            continue

        # Pass each URL on for further page download
        layer_2_pull(r)

def layer_2_pull(url_data):
    '''
    Downloads the html source files listed on the page passed as an argument
    '''

    # BeautifulSoup the page data for searching
    soup = BeautifulSoup(url_data.text, 'html.parser')

    # Locate all URLs from the tables at layer 2
    trs = soup.findAll("tr")

    for tr in trs:
        # Wrap url pull in a try/except in case there is a tr without an href in it
        try:
            #title = tr.td.a.contents[0]
            url = (tr.find('a')['href'])
            print("Layer 2 URL Pull:" + url)
            # Get the folder path of the doc for saving and later parsing
            if 'https://developer.android.com/' in url:
                urlPath= url.replace('https://developer.android.com/', '')

                # Save this page in its relative local folder
                # Credit to Krumelur on stackoverflow.com for the path existence check
                filename = "download/" + urlPath
                if not os.path.exists(os.path.dirname(filename)):
                    try:
                        os.makedirs(os.path.dirname(filename))
                    except OSError as exc: # Guard against race condition
                        if exc.errno != errno.EEXIST:
                            raise

                r = session.get(url)
                with open(filename, 'wb') as outfile:
                    outfile.write(r.text.encode('utf-8'))
        except:
            continue

if __name__ == '__main__':
    # Base URL for git documentation -- list of commands are defined in this
    # base URL and the individual command pages use this base
    # URL + /git-<command_name>
    request = session.get(index)
    layer_1_pull(request)
