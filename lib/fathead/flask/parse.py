#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from bs4 import BeautifulSoup

URL_ROOT = 'http://flask.pocoo.org/docs/0.11/'
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
        for header in self.soup.find_all(['h2']):
            section = self.parse_section(header)
            section['example'] = self.clean_formatting(self.get_example(header))
            if section['paragraph'] and section['example']:
                section['abstract'] = '{}<br><pre><code>{}</code></pre>'.format(section['paragraph'], section['example'])
                self.sections.append(section)
            elif section['paragraph'] and not section['example']:
                section['abstract'] = '{}'.format(section['paragraph'])
                self.sections.append(section)

        self.parsed_data = self.sections

    def get_data(self):
        return self.parsed_data

    def get_example(self, soup):
        text_html = soup.findNext('pre')
        if not text_html is None:
            text = text_html.get_text()
            text = text.strip('\n')
            return text.replace('\n', '\\n')
        else:
            return ""

    def get_url(self, soup):

        link_html = soup.find('a', {'class':'headerlink'}, href=True)
        if not link_html is None:
            anchor=link_html["href"]
        else:
            anchor=''

        if anchor:
            parsed_url = '{}{}'.format(os.path.join(URL_ROOT, self.file_name), anchor)
        else:
            parsed_url = os.path.join(URL_ROOT, self.file_name)

        return parsed_url

    def parse_section(self, soup):
        first_paragraph_html = soup.findNext('p')
        if not first_paragraph_html is None:
            first_paragraph = self.clean_formatting(first_paragraph_html.get_text())
        else:
            first_paragraph=''
        title = self.clean_formatting(soup.get_text())
        anchor = self.get_url(soup)
        url = os.path.join(URL_ROOT, self.file_name)

        return {
            'title': title,
            'paragraph': first_paragraph,
            'anchor': anchor,
            'url': url
        }

    def clean_formatting(self, text):
        
        """
        Flask documentation contains '¶' symbol to represent hyperlinks
        which needs to be removed. Double spaces needs to be 
        converted to single spaces 
        """
        
        text = text.replace('  ', ' ').replace('\n', ' ').replace('\\n', r'\\n').replace("Â¶","").strip()
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

    for filename in os.listdir(DOWNLOADED_HTML_PATH):
        file_path = os.path.join(DOWNLOADED_HTML_PATH, filename)
        data = Data(file_path)
        parser = Parser(data)
        parser.parse_for_data()
        final_data+=parser.get_data()
    output = Writer(final_data)
    output.create_file()


