#!/usr/bin/env
from bs4 import BeautifulSoup

import requests
import os.path

# Shared requests session for quicker pulls
session = requests.Session()

INDEX = 'https://developer.android.com/reference/packages.html'
BASE_URL = 'https://developer.android.com/'


# Layer 1, more to be made for the drill downs
def layer_1_pull(url_data):
    """
    Downloads the html source files listed on the first packages index page
    """
    # BeautifulSoup the page data for searching
    soup = BeautifulSoup(url_data.text, 'html.parser')

    # Locate all URLs from the table at layer 1
    trs = soup.findAll("tr")

    for tr in trs:
        # Wrap url pull in a try/except in case there is a tr without an href in it
        url = (tr.find('a')['href'])
        print("Layer 1 URL Pull:" + url)
        # Get the folder path of the doc for saving and later parsing
        if 'https://developer.android.com/' in url:
            url_path = url.replace('https://developer.android.com/', '')

            # Save this page in its relative local folder
            filename = "download/" + url_path
            if not os.path.exists(os.path.dirname(filename)):
                os.makedirs(os.path.dirname(filename))

            r = session.get(url)
            with open(filename, 'wb') as outfile:
                outfile.write(r.text.encode('utf-8'))

        # Pass each URL on for further page download
        layer_2_pull(r)


def layer_2_pull(url_data):
    """
    Downloads the html source files listed on the page passed as an argument
    """

    # BeautifulSoup the page data for searching
    soup = BeautifulSoup(url_data.text, 'html.parser')

    # Locate all URLs from the tables at layer 2
    trs = soup.findAll("tr")

    for tr in trs:
        # Wrap url pull in a try/except in case there is a tr without an href in it
        url_list = tr.select('td.jd-linkcol a')
        if len(url_list) == 1:
            url = url_list[0]['href']
            print("Layer 2 URL Pull:" + url)
            # Get the folder path of the doc for saving and later parsing
            if 'https://developer.android.com/' in url:
                url_path = url.replace('https://developer.android.com/', '')

                # Save this page in its relative local folder
                filename = "download/" + url_path
                if not os.path.exists(os.path.dirname(filename)):
                    os.makedirs(os.path.dirname(filename))

                r = session.get(url)
                with open(filename, 'wb') as outfile:
                    outfile.write(r.text.encode('utf-8'))


if __name__ == '__main__':
    request = session.get(INDEX)
    layer_1_pull(request)
