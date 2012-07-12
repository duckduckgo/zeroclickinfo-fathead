# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import logging
import os
import re

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()


def replace_all(text, terms):
    """ Replaces all terms contained
    in a dict """
    for _from, _to in terms.items():
        text = text.replace(_from, _to)
    return text


class Package(object):
    """ Contains informations about an AUR package"""
    def __init__(self, name, info, reference):
        self.name = name
        self.info = info
        self.reference = reference
        
    def __str__(self):
        fields = [
                self.name,              # $page
                '',                     # $namespace
                self.reference,         # $url
                self.info,              # $description
                '',                     # $synopsis (code)
                '',                     # $details
                'A',                    # $type
                ''                      # $lang
                ]

        output = '%s' % ('\t'.join(fields))

        return output


class Parser(object):
    """ Parses a HTML file to get
    all packages from it"""
    
    AUR_URL = 'https://aur.archlinux.org'

    def __init__(self, input='download/packages.php'):
        self.soup = BeautifulSoup(open(input), from_encoding='utf-8')

    def get_packages(self):
        """ """
        self.packages = []
        table = self.soup.find('table')
        for row in table.find_all('tr')[1::]:
            data = row.find_all('td')


            name = data[1].span.a.span.getText()
            name = re.sub(' .*', '', name) # getting rid of version 

            info = data[3].span.span.getText().strip()

            reference = self.AUR_URL + '/' + data[1].span.a['href']

            package = Package(name, info, reference)
            self.packages.append(package)
            logger.info('Parsed package %s' % name)


if __name__ == '__main__':
    for file in os.listdir('download/'):
        file = 'download/' + file

        parser = Parser(input=file)
        parser.get_packages()

        with open('output.txt', 'w') as output:
            for package in parser.packages:
                output.write(package.__str__().encode('utf-8') + '\n')
                logger.info('Package added to output: %s' % package.name)
