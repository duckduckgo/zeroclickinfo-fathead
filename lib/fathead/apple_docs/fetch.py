from bs4 import BeautifulSoup
import requests
import os

#Flast documentation root url and download path for all the html files 
URL_ROOT = 'https://developer.apple.com/reference'
SOURCE_URL='https://developer.apple.com'
DOWNLOADED_HTML_PATH = 'download/'

#Shared session object
session = requests.Session()                               

def save_file(link, text):
    name=link.split('/')[-2]+'#'+link.split('/')[-1]
    filename = DOWNLOADED_HTML_PATH+name
    print("downloading %s"%(filename))
    with open(filename, 'w') as f:
        f.write(text)


def parse_links(soup, links_dict):
    class_names=["category-list-item-link", "has-adjacent-element symbol-name"]
    for name in class_names:
        for links in soup.find_all("a", {"class":name}):
            print(links["href"])
            links_dict.append(SOURCE_URL+links["href"])
    return links_dict                         


def fetch_documentation():
        """Parse all the documentation links from URL_ROOT 
        and then recursively download each file 
        and save it in DOWNLOADED_HTML_PATH"""

        sublinks = []
        page_sublinks = []
        curr_page = session.get(URL_ROOT)

        soup = BeautifulSoup(curr_page.text, 'html.parser')
        sublinks = parse_links(soup, sublinks)

        for link in sublinks:
            curr_page = session.get(link)
            soup = BeautifulSoup(curr_page.text, 'html.parser')
            page_sublinks = parse_links(soup, page_sublinks)

        for link in page_sublinks:
            curr_page = session.get(link)
            save_file(link, curr_page.text)

if __name__ == "__main__":
        fetch_documentation()