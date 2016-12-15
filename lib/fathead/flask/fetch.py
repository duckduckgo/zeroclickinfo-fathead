from bs4 import BeautifulSoup
import requests
import os

#Flast documentation root url and download path for all the html files 
URL_ROOT = 'http://flask.pocoo.org/docs/0.11/api'
DOWNLOADED_HTML_PATH = 'download/'

#Shared session object
session = requests.Session()


def fetch_documentation():
        """Parse all the documentation links from URL_ROOT 
        and then recursively download each file 
        and save it in DOWNLOADED_HTML_PATH"""

        curr_page = session.get(URL_ROOT)
        filename = DOWNLOADED_HTML_PATH+'api.html'
        with open(filename, 'w') as f:
                f.write(curr_page.text)


if __name__ == "__main__":
        fetch_documentation()