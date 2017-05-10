#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from bs4 import BeautifulSoup
import html
import unicodedata

URL_ROOT = 'https://developer.apple.com/reference/objectivec'
DOWNLOADED_HTML_PATH = 'download/'

class Data(object):
    """
    Class to store parsed data
    """
    
    def __init__(self, file):
        self.HTML = ""
        self.FILE = file
        self.load_data()

    def load_data(self):
        with open(self.FILE,'r') as data_file:
            document = data_file.read()
            self.HTML = document

    def get_raw_data(self):
        return self.HTML

    def get_file(self):
        return self.FILE


class Parser(object):
    """
    Parser to parse HTML files
    """
    
    def __init__(self, data_object):
        self.file_being_used = data_object.get_file()
        self.file_name = self.file_being_used.split('/').pop()
        self.sections = []
        self.soup = BeautifulSoup(data_object.get_raw_data(), 'html.parser')

    def parse_for_data(self):
        section = {}
        section['title'] = self.soup.find('h1', {'class':'topic-heading'}).text
        section['anchor'] = URL_ROOT + '/' + self.file_name
        section['url'] = URL_ROOT + '/' + self.file_name
        section['paragraph'] = self.soup.find('div', {'class':'topic-abstract abstract formatted-content'})
        section['example'] = self.soup.find('code')

        if section['paragraph']:
            section['paragraph'] = section['paragraph'].text
            section['paragraph'] = section['paragraph'].replace('\n','')
            if section['example']:
                section['example'] = section['example'].text
                section['abstract'] = '<section class="prog__container"><p>{}</p><pre><code>{}</code></pre></section>'.format(section['paragraph'], section['example'])
            else:
                section['abstract'] = '<section class="prog__container"><p>{}</p></section>'.format(section['paragraph'])
        else:
            section['abstract'] = '<section class="prog__container"><p>{}</p></section>'.format('')

        del section['paragraph'] 

        for item in section.keys():
            section[item] = self.clean_formatting(section[item])
        
        self.sections.append(section)
        self.parsed_data = self.sections

    def get_data(self):
        return self.parsed_data

    def clean_formatting(self, text):
        
        """
        Double spaces needs to be 
        converted to single spaces.  
        """
        try:
            text = text.replace('  ', ' ').replace('\n', '\\n').strip()
        except :
            pass
        return text


class Writer(object):
    
    """
    Writes the parsed data to 
    output.txt file in the desired format
    """
    
    def __init__(self, data):
        self.data = data

    def get_data(self, entry_type, title, redirect_data='', url='', abstract='', anchor=''):
        return [
            title,           # title
            entry_type,      # type is article
            redirect_data,   # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            url,             # add an external link back to Dive Into Python
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            anchor           # anchor to specific section
        ]

    def create_file(self):
        
        with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                title = data_element.get('title')
                first_paragraph = data_element.get('first_paragraph')
                abstract = data_element.get('abstract') or first_paragraph
                anchor = data_element.get('anchor')
                url = data_element.get('url')

                list_of_data = []

                entry = self.get_data(
                    'A',
                    title=title,
                    url=url,
                    abstract=abstract,
                    anchor=anchor
                )
                list_of_data.append(entry)

                for data in list_of_data:
                    tsv = '{}\n'.format('\t'.join(data))
                    output_file.write(tsv)


if __name__ == "__main__":
    
    final_data=[]
    for file in os.listdir(DOWNLOADED_HTML_PATH):
        file_path = os.path.join(DOWNLOADED_HTML_PATH, file)
        data = Data(file_path)
        parser = Parser(data)
        parser.parse_for_data()
        final_data+=parser.get_data()
        output = Writer(final_data)
        output.create_file()


