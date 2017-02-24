#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
from bs4 import BeautifulSoup
from bs4 import element

DOWNLOADED_HTML_PATH = 'download/'
REACT_API_DOCS_URL = "https://facebook.github.io/react/docs/"


class HtmlFileData(object):
    """
    Class to Hold Html Data fetched from React Docs
    """
    def __init__(self, file):
        """
        Initialize HtmlFileData object. Load data from Downloaded docs
        """
        self.HTML = ""
        self.FILE = file
        self.load_data()

    def load_data(self):
        """
        Open HTML File and load data
        """
        with open(self.FILE, 'r') as html_file:
            document = html_file.read()
            self.HTML = document

    def get_raw_data(self):
        """
        Returns the Plain HTML
        """
        return self.HTML

    def get_file(self):
        """
        Return File Object
        """
        return self.file


class APIDocsParser(object):
    """
    Parses the HTML Data and fetches
    title , content, example code snippet
    """
    def __init__(self, data):
        """
        Initialize APIDocsParser object with API Reference HTML
        """
        self.data = self.get_api_reference_html(data)
        self.parsed_data = []

    def get_api_reference_html(self, data):
        """
        Parses the HTML File Text and returns the API Reference soup
        """
        soup = BeautifulSoup(data.get_raw_data(), 'html.parser')
        reference = soup.find('a', attrs={'name': 'reference'})
        if reference:
            reference_soup = reference.find_parent().find_parent()
            return reference_soup

    def parse_data(self, file_data):
        """
        Parses Individual API and extracts Title , Link , 
        Content and Example Code
        """
        if self.data:
            all_api_reference = self.data.findAll(["h4","h3"])
            for api in all_api_reference: 
                title = api.text.replace(' #', '')

                href = self.parse_link(file_data, api)

                content = self.parse_content(api)

                example = self.parse_example(api)

                section = {
                    'title': title,
                    'href': href,
                    'content': content,
                    'example': example
                }
                self.parsed_data.append(section)

    def parse_example(self, api):
        """
        Extract the Example Code Snippet
        """
        example_code = ''
        # The example code snippet is inside a pre-tag in a div-tag 
        # Ignore any components that are not tag elements.
        for tag in api.next_siblings:
            if not isinstance(tag, element.Tag):
                continue

            if tag.name == 'div':
                code = str(tag.find('pre'))
                if code != 'None':
                    example_code = tag.text.strip()
        example_code+='\n'      
        return example_code.replace('\n','\\n')

    def remove_anchor(self, code):
        soup = BeautifulSoup(code, 'html.parser')
        for a in soup.findAll('a'):
            a.replaceWith(a.text if a.text!='#' else "")
        return str(soup)

    def parse_content(self, api):
        """
        Extracts the Abstract from API Docs
        """
        abstract = ''
        # This will take all text in p tags to be the abstract.
        # It will break off at the next hr and blockquote tag.
        # Everything that is not a tag element will be ignored 
        # (such as line breaks).
        for tag in api.next_siblings:
            if not isinstance(tag, element.Tag):
                continue

            if tag.name == 'hr' or tag.name == 'blockquote':
                break
            elif tag.name == 'div':
                continue
            else:
                abstract+=(str(tag))
        abstract = self.remove_anchor(abstract)
        abstract+='\n'
        abstract = abstract.replace('\n\n', '\n')
        return abstract.replace('\n', '\\n')

    def parse_link(self,data,api):
        """
        Forms the API Docs Link
        """
        return REACT_API_DOCS_URL + data.FILE.split('/')[1] + api.find('a',attrs = {'class': 'hash-link'}).attrs['href']

    def get_data(self):
        """
        Returns the API List
        """
        return self.parsed_data

class OutputFileData(object):
    """
    Creates the output.txt file using the 
    parsed data
    """
    def __init__(self, api_data, output_file):
        """
        Initialize with parsed api data list
        and name of the output file
        """
        self.data = api_data
        self.output_file = output_file

    def create_file(self):
        """
        Create the output file using the 
        parsed data
        """
        for data_element in self.data:
            title = data_element['title']
            anchor = data_element['href']
            example = data_element['example']
            content = data_element['content']
            if example:
                example = '<pre><code>%s</code></pre>' % example
                abstract = '<section class="prog__container">{}{}</section>'.format(content, example)

            list_of_data = [
                title,                      # api title
                'A',                        # type is article
                '',                         # no redirect data
                '',                         # ignore
                '',                         # no categories
                '',                         # ignore
                '',                         # no related topics
                '',                         # ignore
                '',                         # no external link
                '',                         # no disambiguation
                '',                         # images
                abstract,                   # abstract
                anchor                      # url to doc
            ]
            self.output_file.write('{}\n'.format('\t'.join(list_of_data)))


if __name__ == "__main__":
    with open('output.txt', 'w') as output_file:
        for dir_path, dir_name, file_names in os.walk(DOWNLOADED_HTML_PATH):
            for file_name in file_names:
                print("Parsing %s" % file_name)
                file_path = os.path.join(dir_path, file_name)
                file_data = HtmlFileData(file_path)
                parsed_api_docs = APIDocsParser(file_data)
                parsed_api_docs.parse_data(file_data)
                output_data = OutputFileData(parsed_api_docs.get_data(), output_file)
                output_data.create_file()
