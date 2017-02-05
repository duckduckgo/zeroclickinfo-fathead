#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup

import glob

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
        self.description = description.replace('\n', '\\n').replace('\t', '    ')
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
        self.itext_classes = []
        self.files_to_parse = glob.glob('download/*.html')

    def parse_itext_classes(self):
        self.itext_classes = []

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
                continue

            description = description_list[0].text

            itext_class = ITextClass(name,
                                     description,
                                     file.replace('download/', ''))

            method_blocks = soup.select('.details ul.blockList li.blockList ul.blockList li.blockList ul.blockList li.blockList')
            for method_block in method_blocks:
                method_name = method_block.select('h4')[0].text

                description = None
                for element in method_block.select('.block'):
                    if description is None:
                        description = str(element)
                    else:
                        description += '<br><br>'
                        description += str(element)

                if description is not None:
                    itext_method = ITextClass(name + ' ' + method_name,
                                              description,
                                              file.replace('download/', ''))
                    self.itext_classes.append(itext_method)

            self.itext_classes.append(itext_class)


if __name__ == '__main__':
    # Create the parser
    parser = Parser()

    # Parse the commands
    parser.parse_itext_classes()

    # Write the output for each class into the output.txt file
    with open('output.txt', 'wb') as output:
        for itext_class in parser.itext_classes:
            output.write((str(itext_class) + '\n').encode('utf-8'))