# -*- coding: utf-8 -*-

import logging
import os
import re
import gzip
import string

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()

class Package(object):
    """ Contains informations about an Ubuntu package"""
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

    UBUNTU_PKGS_URL = 'http://packages.ubuntu.com'
    
    def __init__(self, input='download/allpackages?format=txt.gz'):
        self.input = gzip.open(input, 'rb')

        for x in range(6):
            self.input.readline()

    def get_packages(self):
        """ """

        self.packages = []
        for line in self.input:

            if '(' in line:
                data = re.match('(.*?) \(.*?\) (.*)', line).groups()
                name = data[0]
                info = data[1]
            else:
                data = line.split(' ')
                name = data[0]
                info = ' '.join(data[1::])

            # fix for agda-bin package; removing non-ascii characters
            info = filter(lambda x: x in string.printable, info)

            if '[' in info:
                data = re.match('\[(.*?)\] (.*)', info)
                if data:
                    data = data.groups()
                    info = data[1] + ' [' + data[0] + ']'
                else:
                    info = re.sub('\[(.*?)\]', '', info)

            info = info.rstrip('\n')

            reference = self.UBUNTU_PKGS_URL + '/' + name

            package = Package(name, info, reference)
            self.packages.append(package)
            
            logger.info('Parsed package %s' % name)
       
if __name__ == '__main__':
    parser = Parser()
    parser.get_packages()

    with open('output.txt', 'w') as output:
        for package in parser.packages:
            output.write(package.__str__().encode('utf-8') + '\n')
            logger.info('Package added to output: %s' % package.name)
