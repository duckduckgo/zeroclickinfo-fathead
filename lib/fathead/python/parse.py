import os

from bs4 import BeautifulSoup

PYTHON_DOC_BASE_URL = 'https://docs.python.org/3.4{}'
DOWNLOADED_HTML_PATH = 'download/python-3.4.5-docs-html'


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
        self.parsed_data = None
        self.function_sections = []
        self.file_being_used = data_object.get_file()

        soup_data = BeautifulSoup(data_object.get_raw_data(), 'html.parser')
        sections = soup_data.find_all('div', {'class': 'section'})
        
        for section in sections:
            functions = section.find_all('dl', {'class': 'function'})
            if functions:
                self.function_sections.extend(functions)

    def parse_for_module_name(self, section):
        """
        Returns the module name
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            Name of the module

        """
        module_name = section.find('code', {'class': 'descclassname'})
        if module_name:
            return module_name.text.rstrip('.')
        return ''
                                          
    def parse_for_function_name(self, section):
        """
        Returns the function name
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            Name of function

        """
        function_name = section.find('code', {'class': 'descname'})
        if function_name:
            return function_name.text
        return ''

    def parse_for_first_paragraph(self, section):
        """
        Returns the first paragraph of text for a given function
        Fixes up some weird double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            First paragraph found with text

        """
        paragraphs = section.find_all('p')
        for paragraph in paragraphs:
            if paragraph.text:
                return paragraph.text.replace('  ', ' ').replace('\n', ' ')
        return ''

    def parse_for_anchor(self, section):
        """
        Returns the anchor link to specific function doc
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            The href value of the link to doc

        """
        a_tag = section.find('a', {'class': 'headerlink'})
        if a_tag:
            return a_tag['href']
        return ''

    def parse_for_method_signature(self, section):
        """
        Returns the method signature
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            The method signature

        """
        dt = section.find('dt')
        if dt:
            return '<pre><code>{}</code></pre>'.format(dt.text.replace('¶', ''))
        return ''

    def create_url(self, anchor):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the python doc

        """
        file_path = self.file_being_used.replace(DOWNLOADED_HTML_PATH, '')
        return PYTHON_DOC_BASE_URL.format('{}{}'.format(file_path, anchor))

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        
        for function_section in self.function_sections:
            module = self.parse_for_module_name(function_section)
            function = self.parse_for_function_name(function_section)
            if module or function:
                method_signature = self.parse_for_method_signature(function_section)
                first_paragraph = self.parse_for_first_paragraph(function_section)
                anchor = self.parse_for_anchor(function_section)

                url = self.create_url(anchor)

                data_elements = {
                    'module': module,
                    'function': function,
                    'method_signature': method_signature,
                    'first_paragraph': first_paragraph,
                    'url': url
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


class PythonDataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        self.data = data

    def create_names_from_data(self, data_element):
        """
        Figure out the name of the function. Will contain the module name if one exists.
        Args:
            data_element: Incoming data dict

        Returns:
            Name, with whitespace stripped out

        """
        module = data_element.get('module')
        function = data_element.get('function')

        dotted_name = '{}{}{}'.format(module, '.' if module and function  else '', function)
        spaced_name = '{} {}'.format(module, function)

        return dotted_name.strip(), spaced_name.strip()

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'a') as output_file:
            for data_element in self.data:
                if data_element.get('module') or data_element.get('function'):
                    method_signature = data_element.get('method_signature')
                    first_paragraph = data_element.get('first_paragraph')
                    name, redirect = self.create_names_from_data(data_element)

                    abstract = '{}{}{}'.format(method_signature, '<br>' if method_signature and first_paragraph else '', first_paragraph)
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
                        'https://docs.python.org',  # add an external link back to Python home
                        '',                         # no disambiguation
                        '',                         # images
                        abstract,                   # abstract
                        url                         # url to doc
                    ]
                    output_file.write('{}\n'.format('\t'.join(list_of_data)))

                    # Add redirect if we got a redirect name that is different from the original name
                    if redirect != name:
                        list_of_data = [
                            name,                       # unique name
                            'R',                        # type is redirect
                            redirect,                   # redirect alias
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


if __name__ == "__main__":
    for dir_path, dir_name, file_names in os.walk(DOWNLOADED_HTML_PATH):
        for file_name in file_names:
            if '.html' in file_name:
                file_path = '/'.join((dir_path, file_name))
                data = PythonData(file_path)
                parser = PythonDataParser(data)
                parser.parse_for_data()
                output = PythonDataOutput(parser.get_data())
                output.create_file()
