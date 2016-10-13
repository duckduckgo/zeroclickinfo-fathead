# -*- coding: utf-8 -*

from bs4 import BeautifulSoup
import re
import requests
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()

if __name__  == '__main__':
    base_url = 'https://developer.mozilla.org/en-US/docs/Web/HTML/Element/'
    urls = []

    logger.info('Getting Index page')
    response = requests.get(base_url)
    soup_data = BeautifulSoup(response.text, 'html.parser')
    tables = soup_data.find_all('table', {"class":"standard-table"})

    logger.info('Getting Child pages')

    for table in tables:
        for row in table.tbody.find_all('tr'):
            for element in row.td.find_all('a'):
                name = element.string
                name = re.sub(r'[<>]', '', name)
                url = base_url + name
                if url not in urls:
                    urls.append(url)

    list_of_urls = '\n'.join(urls)
    with open('download//list_of_urls.txt', 'w') as f:
        f.write(list_of_urls)