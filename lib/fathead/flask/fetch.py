from bs4 import BeautifulSoup
import requests
import os

#Flast documentation root url and download path for all the html files 
URL_ROOT = 'http://flask.pocoo.org/docs/0.11/'
DOWNLOADED_HTML_PATH = 'download/'

#Shared session object
session = requests.Session()


def fetch_documentation():
        """Parse all the documentation links from URL_ROOT 
        and then recursively download each file 
        and save it in DOWNLOADED_HTML_PATH"""

        flask_urls=[]
        r = session.get(URL_ROOT)
        soup = BeautifulSoup(r.text, 'html.parser')
        for header in soup.find_all('li', {'class':'toctree-l1'}):
                x=header.find('a', href=True)
                curr_url=x["href"].split("#")[0]
                url = os.path.join(URL_ROOT, curr_url)
                curr_page = session.get(url)
                filename = DOWNLOADED_HTML_PATH+str(url.split('/')[-2])+'.html'
                with open(filename, 'w') as f:
                        f.write(curr_page.text)


if __name__ == "__main__":
        fetch_documentation()