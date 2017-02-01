# -*- coding: utf-8 -*-
import os
import csv
from bs4 import BeautifulSoup

INFO = {'download_path': 'download/docs', 
    'doc_base_url': 
            'https://facebook.github.io/react-native/releases/0.40/docs{}',
    'out_file': 'output.txt'}
HOME_LINK= 'http://facebook.github.io/react-native/docs/getting-started.html'
"""
This design is based on the python fathead 
(zeroclickinfo-fathead/lib/fathead/python)

"""
class Data(object):
    """
    Object responsible for loading raw HTML docs:
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


class DataParser(object):
    """
    Object responsible for parsing the raw HTML that contains data
    """
    def __init__(self, data_object, info):
        self.parsed_data = None
        self.prop_sections = []
        self.method_sections = []
        self.intro_text = ''
        self.title = ''
        self.info = info

        self.file_being_used = data_object.get_file()

        soup_data = BeautifulSoup(data_object.get_raw_data(), 'html.parser')
        
        self.title = soup_data.title.text
        
        # Extract intro text
        first_paragraph=soup_data.h1.find_next('p')
        # There is only an intro text for the whole component, if there is not
        #  a h2 before the first paragraph
        if soup_data.h1.find_next('p').find_previous('h2') is None:
            self.intro_text += self._format_output(first_paragraph.text)
        prop_div=soup_data.find('div', {'class': 'props'})
        if prop_div:
            self.prop_sections=prop_div.find_all('div')
            
        # Methods come after a h3 with the text "Methods"
        for h3 in soup_data.find_all('h3'):
            if h3.text=="Methods #":
                props=h3.parent.find('div', {'class': 'props'})
                self.method_sections=props.find_all('div')

    def parse_for_prop_name(self, section):
        """
        Returns the function name
        Args:
            section: A section of parsed HTML that represents a function 
            definition

        Returns:
            Name of function

        """
        prop_name_h4 = section.find('h4', {'class': 'propTitle'})
        # The h4 prop section is consisting of the elements:
        # <a class="anchor"> (Anchor-link), 
        # (optional) <span class="platform"> (platform span element),
        # the name of the prop as clear text,
        # <a class="hash-link"> (hash link)
        link_to_general_props="View props... #"
        if prop_name_h4 and prop_name_h4.text != link_to_general_props:
            prop_name=prop_name_h4.next.next
            if prop_name_h4.find('span', {'class': 'platform'}):
                prop_name=prop_name_h4.find(
                                'span', {'class': 'platform'}).next.next
            
            return prop_name

    def parse_for_first_paragraph(self, section):
        """
        Returns the first paragraph of text for a given function
        Fixes up some weird double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a function 
            definition

        Returns:
            First paragraph found with text

        """
        paragraphs = section.find_all('p')
        for paragraph in paragraphs:
            if paragraph.text:
                return self._format_output(paragraph.text)
        return ''

    def parse_for_anchor(self, section):
        """
        Returns the anchor link to specific function doc
        Args:
            section: A section of parsed HTML that represents a function 
            definition

        Returns:
            The href value of the link to doc

        """
        a_tag = section.find('a', {'class': 'anchor'})
        if a_tag:
            return a_tag['name']
        return ''

    def parse_for_signature(self, section, titleName):
        """
        Returns the signature
        Args:
            section: A section of parsed HTML that represents a definition of 
            a property or method

        Returns:
            The signature

        """
        h4 = section.find('h4', {'class': titleName})
        contents=[]
        
        for e in h4.strings:
            contents.append(e)
        # Remove the last item (and the preceding space), it is a hash link
        del contents[-1]
        del contents[-1]
        # If platform is present, remove it - relevant for Properties
        if h4.find('span', {'class': 'platform'}):
            del contents[0]
        # If there are two spans with class methodType, the first is not wanted,
        # because it is "static".
        # Relevant for methods section
        if len(h4.find_all('span', {'class': 'methodType'})) > 1:
            del contents[0]
        if contents:
            signature=''
            for el in contents:
                signature+=el
            return '<pre><code>{}</code></pre>'.format(
                self._format_output(signature))
        return ''

    def parse_for_method_name(self, section):
        """
        Returns the name of a method
        Args:
            section: A section of parsed HTML that represents a method definition

        Returns:
            The method name

        """
        method_name_h4 = section.find('h4', {'class': 'methodTitle'})
        # The h4 method name section is consisting of the elements:
        # <a class="anchor"> (Anchor-link), 
        # <span class="methodType"> (method type span element),
        # the name of the prop as clear text,
        # <span class="methodType"> (method signature span element),
        # <a class="hash-link"> (hash link)
        if method_name_h4:
            method_name=method_name_h4.next.next
            nbr_of_methodType_tags_in_h4=len(method_name_h4.find_all(
                                    'span', {'class': 'methodType'}))
            if nbr_of_methodType_tags_in_h4 > 1:
                method_name=method_name_h4.find(
                                    'span', {'class': 'methodType'}).next.next
            
            return method_name

    def create_url(self, anchor):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the python doc

        """
        file_path = self.file_being_used.replace(self.info['download_path'], '')
        return self.info['doc_base_url'].format(
                                            '{}#{}'.format(file_path, anchor))

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data 
        elements.
        """
        data = []

        if self.intro_text and self.title:
            data_elements = {
                'module': self.title,
                'function': '',
                'method_signature': '',
                'first_paragraph': self.intro_text,
                'url': self.create_url('')
            }
            data.append(data_elements)

        titleName='propTitle'
        for prop_section in self.prop_sections:
            prop_name = self.parse_for_prop_name(prop_section)
            if prop_name:
                prop_signature = self.parse_for_signature(prop_section,
                                                                    titleName)
                first_paragraph = self.parse_for_first_paragraph(prop_section)
                anchor = self.parse_for_anchor(prop_section)
                url = self.create_url(anchor)
                data_elements = {
                    'module': self.title,   # "Module" is another name for
                                            # "component" or "API" in this
                                            # fathead
                    'function': prop_name,
                    'method_signature': prop_signature,
                    'first_paragraph': first_paragraph,
                    'url': url,
                }

                data.append(data_elements)
        
        titleName='methodTitle'
        for method_section in self.method_sections:
            method_name=self.parse_for_method_name(method_section)
            if method_name:
                method_signature = self.parse_for_signature(method_section,
                                                                    titleName)
                first_paragraph = self.parse_for_first_paragraph(method_section)
                anchor = self.parse_for_anchor(method_section)
                url = self.create_url(anchor)

                data_elements = {
                    'module': self.title,
                    'function': method_name,
                    'method_signature': method_signature,
                    'first_paragraph': first_paragraph,
                    'url': url,
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

    def _format_output(self, text):
        """
            Helper method to format the output appropriately.
        """
        return text.replace('  ', ' ').replace('\n', ' ').replace('\\n', r'\\n')


class DataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        self.data = data
        self.output = INFO['out_file']

    def create_names_from_data(self, data_element):
        """
        Figure out the name of the function. Will contain the module name if 
        one exists.
        Args:
            data_element: Incoming data dict

        Returns:
            Name, with whitespace stripped out

        """
        module = data_element.get('module')
        function = data_element.get('function')

        dotted_name = '{}{}{}'.format(module, '.' 
                                      if module and function  else '', function)
        spaced_name = '{} {}'.format(module, function)

        return dotted_name.strip(), spaced_name.strip()

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, 
        appending to file as necessary.

        """
        with open(self.output, 'a') as output_file:
            for data_element in self.data:
                if data_element.get('module') or data_element.get('function'):
                    method_signature = data_element.get('method_signature')
                    first_paragraph_text=data_element.get('first_paragraph')
                    first_paragraph=''
                    if (first_paragraph_text):
                        first_paragraph='<p>'
                        first_paragraph+=first_paragraph_text
                        first_paragraph+='</p>'

                    name, redirect = self.create_names_from_data(data_element)

                    abstract='<section class="prog__container">'
                    abstract+='{}{}{}'.format(first_paragraph,
                                                '' , 
                                                method_signature)
                    abstract+='</section>'
                    url = data_element.get('url')
                    list_of_data = [
                        name,                       # unique name
                        'A',                        # type is article
                        '',                         # no redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        HOME_LINK,                  # add an external link back to react native home
                        '',                         # no disambiguation
                        '',                         # images
                        abstract,                   # abstract
                        url                         # url to doc
                    ]
                    output_file.write('{}\n'.format('\t'.join(list_of_data)))

                    # Add redirect if we got a redirect name that is different from the original name
                    if redirect != name:
                        list_of_data = [
                            redirect,                   # unique name
                            'R',                        # type is redirect
                            name,                       # redirect alias, to the original data
                            '',                         # ignore
                            '',                         # no categories
                            '',                         # ignore
                            '',                         # no related topics
                            '',                         # ignore
                            '',                         # no external link
                            '',                         # no disambiguation
                            '',                         # images
                            '',                         # no abstract
                            ''                          # no url
                        ]
                        output_file.write('{}\n'.format('\t'.join(list_of_data)))


def cleanup(out_file):
    """
    Cleanup output.txt's files.  Mostly for use during local dev/testing.
    """
    if os.path.isfile(out_file):
        os.remove(out_file)


if __name__ == "__main__":
    cleanup('output.txt')
    for dir_path, dir_name, file_names in os.walk(INFO['download_path']):
        for file_name in file_names:
            if '.html' in file_name:
                print("Processing %s " % file_name)
                file_path = '/'.join((dir_path, file_name))
                data = Data(file_path)
                parser = DataParser(data, INFO)
                parser.parse_for_data()
                output = DataOutput(parser.get_data())
                output.create_file()
