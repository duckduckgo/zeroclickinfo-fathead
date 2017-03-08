# -*- coding: utf-8 -*-
import os
import csv
from bs4 import BeautifulSoup

DJANGO_HOME = 'https://www.djangoproject.com/'
DJANGO_DOC_URL = 'https://docs.djangoproject.com/en/1.10/ref{}'

INFO = {'download_path': 'download/docs.djangoproject.com/en/1.10/ref', 
    'doc_base_url': 
            DJANGO_DOC_URL,
    'out_file': 'output.txt'}
HOME_LINK= 'https://docs.djangoproject.com/en/1.10/'
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
        Initialize Data object. Load data from HTML.

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

    def get_file_path(self):
        """
        Returns: The file path of the file being used.

        """
        return self.FILE


class DataParser(object):
    """
    Object responsible for parsing the raw HTML that contains data
        
    """
    
    """
        Args:
            soup_data: BeautifulSoup data object.
            file_name: file name of object being parsed.
            info: Information about the document (url).
            module_pre: If this is a submodule, this contains the parent module.
    """
    def __init__(self, soup_data, file_name, info, module_pre=''):
        self.parsed_data = None
        self.class_sections = []
        self.method_sections = []
        self.intro_text = ''
        self.module = module_pre
        self.info = info
        self.soup_data = soup_data
        self.file_being_used = file_name

    def _parse_for_element_name(self, section):
        """
        Returns the function name
        Args:
            section: A section of parsed HTML that represents a function 
            definition

        Returns:
            Name of function

        """
        function_name = section.find('code', {'class': 'descname'}).text
        return function_name

    def _parse_for_first_paragraph(self, section):
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

    def _parse_for_anchor(self, section):
        """
        Returns the anchor link to specific function doc
        Args:
            section: A section of parsed HTML that represents a function 
            definition

        Returns:
            The href value of the link to doc

        """
        a_tag = section.find('a', {'class': 'headerlink'})
        if a_tag:
            return a_tag['href']
        return ''

    def _parse_for_signature(self, section):
        """
        Returns the signature
        Args:
            section: A section of parsed HTML that represents a definition of 
            a class, method or function

        Returns:
            The signature
        """
        contents=[]
        for e in section.dt.strings:
            contents.append(e)
        if contents:
            del contents[-1]
            if '[source]' in contents:
                del contents[-1]
            signature=''
            for el in contents:
                signature+=el
            return '<pre><code>{}</code></pre>'.format(
                    self._format_output(signature))
        return ''

    def _create_url(self, anchor):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the doc

        """
        file_path = self.file_being_used.replace(
                    self.info['download_path'], '').replace('/index.html', '')
        return self.info['doc_base_url'].format('{}/{}'.format(file_path, anchor))

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all neccessary data
        elements.
        """
        self.parsed_data = []
        # Extract intro text
        first_paragraph=''
        div_section=soup_data.find('div', {'class': 'section'})
        if div_section:
            self._parse_section(div_section, self.module)

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
    

    def _parse_section(self, div_section, module_name):
        """
        Helper method to parse section, and recursively go through sub sections
            Args:
                div_section: The section div of the module
                module_name: The name of the module
            Returns:
                Nothing - but stores data in the self.parsed_data list.
        """
        title_id=div_section.get('id')
        title_split=title_id.split('-')
        module_titles=title_split[-1].split('.')
        # The last item in module_titles is the module for this section
        # Require that there is a dot in the module title, else, it is
        # not a sub module - only a sub section in the html document
        if len(module_titles) > 1:
            if module_name:
                module_name=module_name+'.'+module_titles[-1]
            else:
                module_name=module_titles[-1]
        
        first_paragraph=div_section.find_next('p')
        intro_text=''
        if first_paragraph:
            intro_text = self._format_output(first_paragraph.text)
            
        section_anchor=self._parse_for_anchor(div_section)
        if intro_text and module_name:
            data_elements = {
                'module': module_name,
                'function': '',
                'method_signature': '',
                'first_paragraph': intro_text,
                'url': self._create_url(section_anchor)
            }
            self.parsed_data.append(data_elements)   
        sub_sections = div_section.find_all('div', {'class': 'section'})
        if sub_sections:
            for sub_section in sub_sections:
                # Parse each sub section
                self._parse_section(sub_section, module_name)
        # Now, replace all sub sections, that have already been parsed
        sub_sections = div_section.find_all('div', {'class': 'section'})
        [ sub_section.replaceWith('') for sub_section in sub_sections ]
        

        self._parse_section_for_elements(div_section, module_name)

    def _parse_section_for_elements(self, section, module_name):
        # Extract classes, methods and functions.
        class_sections=section.find_all('dl', {'class': 'class'})
        if class_sections: 
            self._parse_for_elements(class_sections, module_name)           
        method_sections=section.find_all('dl', {'class': 'method'})
        if method_sections:
            self._parse_for_elements(method_sections, module_name)            
        function_sections=section.find_all('dl', {'class': 'function'})
        if function_sections:
            self._parse_for_elements(function_sections, module_name)
        
        # Parse for attributes
        attribute_sections=section.find_all('dl', {'class': 'attribute'})
        if attribute_sections:
            self._parse_for_attributes(attribute_sections)
        
    def _parse_for_elements(self, sections, module_name):
        """
        Method for parsing out classes, methods or function elements from a
            section. The data will be stored to self.parsed_data
        Args:  
            function_sections: The sections to be parsed
            module_name: The name of the module
        Returns:
            nothing, but stores data in self.parsed_data
        
        """
        for section in sections:
            name=self._parse_for_element_name(section)
            signature=self._parse_for_signature(section)
            first_paragraph=self._parse_for_first_paragraph(section)
            anchor=self._parse_for_anchor(section)
            data_element = {
                    'module': module_name,
                    'function': name,
                    'method_signature': signature,
                    'first_paragraph': first_paragraph,
                    'url': self._create_url(anchor),
            }
            self.parsed_data.append(data_element)
    
    def _parse_for_attributes(self, attribute_sections):
        for section in attribute_sections:            
            name=self._parse_for_element_name(section)        
            full_class_name = section.dt.get('id')
            if full_class_name is None:
                return
            parts=full_class_name.split('.')
            del parts[0]
            del parts[-1]
            class_name = '.'.join(parts)
            signature=self._parse_for_signature(section)
            first_paragraph=self._parse_for_first_paragraph(section)
            anchor=self._parse_for_anchor(section)
            data_element = {
                    'module': class_name,
                    'function': name,
                    'method_signature': signature,
                    'first_paragraph': first_paragraph,
                    'url': self._create_url(anchor),
            }
            self.parsed_data.append(data_element)
        
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
                print("Processing %s/%s " % (dir_path, file_name))
                file_path = '/'.join((dir_path, file_name))
                data = Data(file_path)
                soup_data = BeautifulSoup(data.get_raw_data(), 'html.parser')
                parser = DataParser(soup_data, data.get_file_path(), INFO)
                parser.parse_for_data()
                output = DataOutput(parser.get_data())
                output.create_file()
