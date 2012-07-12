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
    """ Contains informations about an Arch package"""
    def __init__(self, name, info, reference, arch):
        self.name = name
        self.info = info
        self.reference = reference
        self.arch = arch
        
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

    ARCHLINUX_URL = 'https://www.archlinux.org'
    
    def __init__(self, input='download/index.html?limit=all'):
        self.soup = BeautifulSoup(open(input), from_encoding='utf-8')

    def get_packages(self):
        """ """
        self.packages = []

        table = self.soup.find('table')

        for row in table.find_all('tr')[1::]:
            data = row.find_all('td')

            name = data[2].a.getText()
            reference = self.ARCHLINUX_URL + data[2].a['href']
            info = data[4].getText()
            arch = data[0].getText()

            package = Package(name, info, reference, arch)
            self.packages.append(package)

            # removing duplicates
            if len(self.packages) >= 2:
                if self.packages[-1].name == self.packages[-2].name: 
                    del self.packages[-1]

            #logger.info('Parsed package %s' % name)
       
if __name__ == '__main__':
    parser = Parser()
    parser.get_packages()

    with open('output.txt', 'w') as output:
        for package in parser.packages:
            output.write(package.__str__().encode('utf-8') + '\n')
            #logger.info('Package added to output: %s' % package.name)
