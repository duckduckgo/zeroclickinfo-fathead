# -*- coding: utf-8 -*

from bs4 import BeautifulSoup
import re
import requests
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()


def is_element(url):
    """
    Return whether a given url points to a 
    valid HTML ELEMENT Reference Page.
    """
    element_url_prefix = "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/"
    return url.startswith(element_url_prefix)


if __name__  == '__main__':
    doc_urls = [
        'https://developer.mozilla.org/en-US/docs/Web/HTML/Element',
        'https://developer.mozilla.org/en-US/docs/Web/HTML/Index'
        ]
    base_url = 'https://developer.mozilla.org'
    urls = []

    logger.info('Getting Index pages')
    for doc_url in doc_urls:
        response = requests.get(doc_url)
        soup_data = BeautifulSoup(response.text, 'html.parser')
        tables = soup_data.find_all('table', {"class":"standard-table"})

        for table in tables:
            for row in table.tbody.find_all('tr'):
                for td in row.find_all('td'):
                    for element in td.find_all('a'):
                        name = element.text
                        name = re.sub(r'[<>]', '', name)
                        relative_url = element['href']
                        url = base_url + relative_url
                        if url not in urls and is_element(url):
                            urls.append(url)

    list_of_urls = '\n'.join(urls)
    with open('download//list_of_urls.txt', 'w') as f:
        f.write(list_of_urls)
    
    logger.info('Getting Child pages')