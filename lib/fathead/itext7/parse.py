#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import re
from urllib.parse import urljoin


import glob

METHODS = 2

itext_docs_base_url = open('data.url').read().strip()


class ITextClass(object):
    """
        Class for storing all necessary information about an iText Class. This
        information is then used to create the output.txt file using the
        __str__() method on this class
    """

    def __init__(self, name, description, filename):
        """Instantiate the information about the class"""
        self.name = name
        self.description = description.replace('\n', '').replace('\t', '    ')
        self.description = '<p>{}</p>'.format(self.description)
        self.filename = filename

    def __str__(self):
        abstract = '<section class="prog__container">{}</section>'.format(self.description)

        return '\t'.join([
            self.name,  # Full article title
            'A',  # Type of article
            '',  # For redirects only
            '',  # Ignore
            '',  # Categories
            '',  # Ignore
            '',  # Related Topics
            '',  # Ignore
            '',  # External links
            '',  # For disambiguation pages only
            '',  # Image
            abstract,  # Abstract
            '{}{}'.format(itext_docs_base_url,
                          self.filename),  # URL
        ])


class Parser(object):
    def __init__(self):
        """Get all itext class files that need to be parsed"""
        self.itext_classes = {}
        self.files_to_parse = glob.glob('download/*.html')

    def parse_itext_classes(self):
        self.itext_classes = {}

        for file in self.files_to_parse:
            print(file)
            soup = BeautifulSoup(open(file), 'html.parser')

            title_list = soup.select('title')

            if len(title_list) != 1:
                continue

            title = title_list[0].text

            # Titles take the format "<Classname> (iText 7 7.0.1 API)" so we strip the second part out
            name = title.split()[0]

            description_list = soup.select('div.contentContainer div.description div')

            if len(description_list) != 1:
                description = ''

            else:

                description = description_list[0].text

            class_path = soup.select('.header .subTitle')[0].text
            class_path = class_path.replace('.', '/')
            page_link = class_path + '/' + name + '.html'

            itext_class = ITextClass(name,
                                     description,
                                     page_link)

            method_details = soup.find(text=re.compile(r'Method Detail'))

            if method_details is not None:
                method_details = method_details.parent.parent.parent

                method_blocks = method_details.select('li.blockList > ul')
                method_anchors = method_details.select('li.blockList > a')[1:] #First anchor just links to method section

                for method_details, anchor in zip(method_blocks, method_anchors):
                    method_name = method_details.select('h4')[0].text

                    for a in method_details.findAll('a'):
                        a['href'] = urljoin(itext_docs_base_url + page_link, a['href'])

                    description = ''
                    for element in method_details.select('.block'):
                        copied = element.select('span.descfrmTypeLabel')
                        if len(copied) > 0:
                            copied[0].name = 'b'
                        if len(description) == 0:
                            description += str(element)
                        else:
                            description += '<br>'
                            description += str(element)

                    header = method_details.select('li.blockList > pre')

                    if len(header) > 0:
                        description += re.sub(r'([,\)])\s+', r'\1 ', str(header[0]))

                    if description is not None:
                        method = name + ' ' + method_name
                        itext_method = ITextClass(method,
                                                  description,
                                                  page_link + '#' + anchor['name'])
                        if method not in self.itext_classes:
                            self.itext_classes[method] = itext_method
                        elif len(self.itext_classes[method].description) < len(description):
                            self.itext_classes[method] = itext_method

            self.itext_classes[name] = itext_class


if __name__ == '__main__':
    # Create the parser
    parser = Parser()

    # Parse the commands
    parser.parse_itext_classes()

    # Write the output for each class into the output.txt file
    with open('output.txt', 'wb') as output:
        for itext_class in parser.itext_classes.values():
            output.write((str(itext_class) + '\n').encode('utf-8'))
