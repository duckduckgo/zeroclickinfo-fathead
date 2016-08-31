#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup, NavigableString

SOUP_HOME = 'https://www.crummy.com/software/BeautifulSoup/bs4/doc/'

class SoupData(object):
    """
    Object responsible for loading raw HTML data from BeautifulSoup doc:

    """

    def __init__(self):
        """
        Initialize SoupData object. Load data from HTML.

        """
        self.SOUP_HTML = ""
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.

        """
        with open('download/index.html', 'r') as data_file:
            self.SOUP_HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.

        """
        return self.SOUP_HTML

class SoupDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains the beautifulsoup data

    """

    def __init__(self, raw_data):
        """
        Given raw data, get the relevant section
        Args:
            raw_data: HTML data
        """
        self.parsed_data = None
        self.sections = list()

        soup_data = BeautifulSoup(raw_data, 'html.parser')
        for content in soup_data.find('div', {'class': 'body'}):
            if not content or isinstance(content, NavigableString):
                continue
            self.sections.append(content)

    def parse_name_and_anchor_from_data(self, section):
        """
        Find the name and anchor for a given section
        Args:
            section: A section of parsed HTML

        Returns:
            name: Name of the section
            anchor: Anchor tag to use when linking back to the docs (ie #tags)
        """
        name, anchor = None, None
        a_tag = section.find('a', {'class': 'headerlink'})
        if a_tag:
            anchor = a_tag['href']
            name = "".join(anchor[1:])
        return name, anchor

    def parse_first_paragraph_from_data(self, section):
        """
        Get the first paragraph for display
        Args:
            section: A section of parsed HTML

        Returns:
            First paragraph in the HTML
        """
        data = section.find('p')
        if data:
            return data.text.replace('\n', ' ').replace('  ', ' ')
        return None

    def parse_code_from_data(self, section):
        """
        Look for example code block to output
        Args:
            section: A section of parsed HTML

        Returns:
            Formatted code string
        """
        code = section.find('div', {'class': 'highlight-python'})
        if code:
            return '<pre><code>{}</code></pre>'.format(code.text.replace('¶', '').replace('\n', '\\n'))
        return None

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = list()

        for section in self.sections:
            for sub_section in section.find_all('div', {'class': 'section'}):
                name, anchor = self.parse_name_and_anchor_from_data(sub_section)
                first_paragraph = self.parse_first_paragraph_from_data(sub_section)
                code = self.parse_code_from_data(sub_section)
                if name and first_paragraph and code:
                    data_elements = {
                        'name': name,
                        'anchor': anchor,
                        'first_paragraph': first_paragraph,
                        'code': code
                    }

                    data.append(data_elements)
        self.parsed_data = data

    def get_data(self):
        """
        Get the parsed data.
        Returns:
            self.parsed_data: Dict containing necessary data elements
        """
        return self.parsed_data


class SoupDataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        """
        Initialize SoupDataOutput object
        Args:
            data: Dict containing the data elements
        """
        self.data = data

    def create_file(self):
        """
        Iterate through the data and create the necessary output.txt file
        """

        with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                name = data_element.get('name')
                code = data_element.get('code')
                first_paragraph = data_element.get('first_paragraph')
                abstract = '{}{}{}'.format(code, '<br>' if code else '', first_paragraph)
                url = '{}{}'.format(SOUP_HOME, data_element.get('anchor'))
                list_of_data = [
                    name,        #unique name
                    'A',         #type is article
                    '',          #no redirect data
                    '',          #ignore
                    '',          #no categories
                    '',          #ignore
                    '',          #no related topics
                    '',          #ignore
                    SOUP_HOME,   #add an external link back to BeautifulSoup Home
                    '',          #no disambiguation
                    '',          #images
                    abstract,    #abstract
                    url          #url to the relevant tag doc
                ]
                output_file.write('{}\n'.format('\t'.join(list_of_data)))


if __name__ == "__main__":
    data = SoupData()
    parser = SoupDataParser(data.get_raw_data())
    parser.parse_for_data()
    output = SoupDataOutput(parser.get_data())
    output.create_file()
