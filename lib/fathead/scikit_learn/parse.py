#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from bs4 import BeautifulSoup

URL_ROOT = 'http://scikit-learn.org/stable/auto_examples/index.html'
DOWNLOADED_HTML_PATH = 'download/'

class PythonData(object):
    """
    Object responsible for loading raw HTML data from Python docs:
    """
    def __init__(self, file):
        """
        Initialize PythonData object. Load data from HTML.
        """
        self.HTML = ""
        self.FILE = file
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.

        """
        with open(self.FILE, 'r') as data_file:
            self.HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.

        """
        return self.HTML

    def get_file(self):
        """
        Returns: The file path of the file being used.

        """
        return self.FILE


class PythonDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains Python data
    """
    def __init__(self, data_object):
        """
        Given raw data, get the relevant sections
        Args:
            raw_data: HTML data
        """
        self.parsed_data = []
        self.soup = BeautifulSoup(data_object.get_raw_data(), 'html.parser')

    def parse_for_data(self):
        """
        Main loop for parsing main subjects and their sub sections
        """

        body_section = self.soup.find('div', {'class': 'section'})

        # Section title
        title = self.parse_title(body_section)

        # First paragraph
        first_paragraph = self.parse_for_first_paragraph(body_section)

        # Example
        example = self.parse_example(body_section)

        # content
        content = self.parse_content(body_section)

        # Get canonical link
        href = self.soup.find('link', {'rel': 'canonical'}).get('href')

        section = {
            'title': title,
            'content': content,
            'first_paragraph': first_paragraph,
            'example': '<br><pre><code>{}</code></pre>'.format(example),
            'anchor': href
        }
        self.parsed_data.append(section)

    def parse_example(self, body_soup):
        """
        Returns code example for block

        First searches if there are highlighted divs, then seeks out the correct
        div for code example.
        Args:
            body_soup: BeautifulSoup object containing section body
        Returns:
            Code block
        """
        highlight = body_soup.find('div', {'class': 'highlight-python'})
        if highlight:
            # Code div we want is the next one after text 'Python source code'
            for p in body_soup.find_all('p'):
                content = p.get_text()
                if 'source code' in content:
                    text = p.findNext('div', {'class': 'highlight'}).get_text()
                    return text.strip('\n')
        return ''

    def parse_title(self, body_soup):
        """
        Return content title
        Args:
            body_soup: BeautifulSoup object containing section body
        Returns:
            Page title
        """
        return str(body_soup.find('h1').get_text()).replace('¶', '')

    def parse_content(self, body_soup):
        """
        Returns all paragraphs as text, joined by two new lines
        Args:
            body_soup: BeautifulSoup object containing section body
        Returns:
            All paragraphs as text
        """
        paragraphs = body_soup.find_all('p')
        content = []
        for paragraph in paragraphs:
            content.append(paragraph.get_text())
        return '\n\n'.join(content)

    def parse_for_first_paragraph(self, section):
        """
        Returns the first paragraph of text for a given function
        Fixes up double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            First paragraph found with text

        """
        paragraphs = section.find_all('p')
        for paragraph in paragraphs:
            if paragraph.text:
                return paragraph.text.replace('  ', ' ').replace('\n', ' ').replace('\\n', r'\\n')
        return ''

    def get_data(self):
        """
        Get the parsed data.
        Returns:
            self.parsed_data: Dict containing necessary data elements
        """
        return self.parsed_data


class PythonDataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data, output_file):
        self.data = data
        self.output_file = output_file

    def truncate(self, text, length):
        if len(text) <= length:
            return text
        return "{}...".format(text[:length].rsplit(' ', 1)[0])

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.
        """
        for data_element in self.data:
            title = data_element.get('title')
            abstract = self.truncate(data_element.get('first_paragraph'), 400)
            example = data_element.get('example')
            if example:
                abstract = "{}<br>{}".format(abstract, example)
            abstract = abstract.replace('\n', '\\n')
            anchor = data_element.get('anchor')

            list_of_data = [
                title,                      # unique name
                'A',                        # type is article
                '',                         # no redirect data
                '',                         # ignore
                '',                         # no categories
                '',                         # ignore
                '',                         # no related topics
                '',                         # ignore
                URL_ROOT,                   # add an external link back to Scikit home
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
                file_path = os.path.join(dir_path, file_name)
                data = PythonData(file_path)
                parser = PythonDataParser(data)
                parser.parse_for_data()
                output = PythonDataOutput(parser.get_data(), output_file)
                output.create_file()
