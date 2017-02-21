#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import re
from urllib.parse import urljoin
import glob

itext_docs_base_url = 'http://itextsupport.com/apidocs/itext7/7.0.1/'


class ITextFathead(object):
    """
        Class for storing all necessary information about an iText Fathead entry. This
        information is then used to create the output.txt file using the
        __str__() method on this class
    """

    def __init__(self, name, description, filename):
        """
        :param name: Name of the Fathead
        :param description: Description of what's being displayed
        :param filename: The filename from which the information came. Used to construct the URL for the entry
        Instantiate the information about the class
        """
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

    def create_output(self):
        self.parse_itext_classes()
        self.write_classes_to_file('output.txt')

    def parse_itext_classes(self):
        self.itext_classes = {}

        for file in self.files_to_parse:
            print(file)
            soup = BeautifulSoup(open(file), 'html.parser')

            class_name = self.get_class_name(file, soup)
            description = self.get_class_description(soup)
            page_link = self.create_page_link(class_name, soup)

            if description:
                itext_class = ITextFathead(class_name,
                                           description,
                                           page_link)
                self.itext_classes[class_name] = itext_class
            self.parse_methods_from_class(class_name, page_link, soup)

    def parse_methods_from_class(self, class_name, page_link, soup):
        method_details = soup.find(text=re.compile(r'Method Detail'))
        if method_details is not None:
            method_details = method_details.parent.parent.parent

            method_blocks = method_details.select('li.blockList > ul')
            method_anchors = method_details.select('li.blockList > a')[1:]  # First anchor just links to method section

            for method_details, anchor in zip(method_blocks, method_anchors):
                method_name = method_details.select('h4')[0].text

                self.remove_anchor_tags(method_details)

                description = self.get_method_description(method_details)

                itext_method = ITextFathead(class_name + ' ' + method_name,
                                            description,
                                            page_link + '#' + anchor['name'])

                self.add_method_to_output(itext_method)

    def add_method_to_output(self, itext_method):
        name = itext_method.name
        if name not in self.itext_classes:
            self.itext_classes[name] = itext_method
        elif len(self.itext_classes[name].description) < len(itext_method.description):
            self.itext_classes[name] = itext_method

    @staticmethod
    def get_class_name(file, soup):
        title_list = soup.select('title')
        if len(title_list) != 1:
            raise Exception('Page format for {} not know'.format(file))
        title = title_list[0].text
        # Titles take the format "<Classname> (iText 7 7.0.1 API)" so we strip the second part out
        name = title.split()[0]
        return name

    @staticmethod
    def get_class_description(soup):
        description_list = soup.select('div.contentContainer div.description div')
        if len(description_list) != 1:
            description = None 
        else:
            description = description_list[0].text
        return description

    @staticmethod
    def create_page_link(class_name, soup):
        class_path = soup.select('.header .subTitle')[0].text
        class_path = class_path.replace('.', '/')
        page_link = class_path + '/' + class_name + '.html'
        return page_link

    @staticmethod
    def remove_anchor_tags(method_details):
        for a in method_details.findAll('a'):
            a.unwrap()

    @staticmethod
    def get_method_description(method_details):
        description = ''
        for element in method_details.select('div.block'):
            copied = element.select('span.descfrmTypeLabel')
            if len(copied) > 0:
                copied[0].name = 'b'
            if len(description) > 0:
                description += '<br>'
            description += str(element.decode_contents(formatter="html"))

        header = method_details.select('li.blockList > pre')
        if len(header) > 0:
            description += re.sub(r'([,)])\s+', r'\1 ', str(header[0]))
        return description

    def write_classes_to_file(self, filename):
        # Write the output for each class into the output.txt file
        with open(filename, 'wb') as output:
            for itext_class in self.itext_classes.values():
                output.write((str(itext_class) + '\n').encode('utf-8'))


if __name__ == '__main__':
    parser = Parser()
    parser.create_output()
