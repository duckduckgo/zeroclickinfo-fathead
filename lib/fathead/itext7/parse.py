#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup

import glob

itext_docs_base_url = open('data.url').read().strip()



class Parser(object):
    def __init__(self):
        """Get all itext class files that need to be parsed"""
        self.files_to_parse = glob.glob('download/*.html')

    def parse_itext_classes(self):
        self.itext_classes = []

        for file in self.files_to_parse:
            soup = BeautifulSoup(open(file), 'html.parser')

            name = soup.select('h2.title').text

            if not name:
                continue

            description_list = soup.select('div.contentContainer div.description div')

            if len(description_list) != 1:
                continue

            description = description_list[0].text

            itext_class = ITextClass(name, description)


if __name__ == '__main__':
    # Create the parser
    parser = Parser()

    # Parse the commands
    parser.parse_itext_classes()

    # Write the output for each class into the output.txt file
    with open('output.txt', 'wb') as output:
        for itext_class in parser.itext_classes:
            output.write((itext_class.basic_usage() + '\n').encode('utf-8'))