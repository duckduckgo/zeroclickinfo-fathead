import os

from bs4 import BeautifulSoup

PYTHON_DOC_BASE_URL = 'http://sass-lang.com/documentation/Sass/Script/Functions.html'
DOWNLOADED_HTML_PATH = 'download/Functions.html'


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
        sections = soup_data.find_all('dl', {'class': None})
        for section in sections:
            function_names = section.find_all('dt')
            function_descriptions = section.find_all('dd')
            for i in range(len(function_names)):
                self.function_sections.append([function_names[i], function_descriptions[i]])
                             
    def parse_for_function_name(self, section):
        """
        Returns the function name
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            Name of function

        """
        function_name = section.find('a')
        if function_name:
            #return the module name without paramaters
            return function_name.text.split('(')[0]
        return ''

    def parse_for_description(self, section):
        """
        Returns the function description
        Fixes up some weird double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a function description

        Returns:
            Function description

        """
        
        return section.text.replace('  ', ' ').replace('\n', ' ').replace('\\n', r'\\n')

    def parse_for_anchor(self, section):
        """
        Returns the anchor link to specific function doc
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            The href value of the link to doc

        """
        a_tag = section.find('a')
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
        method_sig = section.find('a')
        if method_sig:
            #return the function name with paramaters
            return '<pre><code>'+method_sig.text+'</code></pre>'
        return ''

    def create_url(self, anchor):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the python doc

        """
        return PYTHON_DOC_BASE_URL + anchor

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        
        for function_section in self.function_sections:
            function = self.parse_for_function_name(function_section[0])
            if function:
                method_signature = self.parse_for_method_signature(function_section[0])
                description = self.parse_for_description(function_section[1])
                anchor = self.parse_for_anchor(function_section[0])

                url = self.create_url(anchor)

                data_elements = {
                    'function': function,
                    'method_signature': method_signature,
                    'description': description,
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

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'w') as output_file:
            for data_element in self.data:
                if data_element.get('function'):
                    method_signature = data_element.get('method_signature').encode('utf-8')
                    description = data_element.get('description').encode('utf-8')
                    name = data_element.get('function').encode('utf-8')

                    abstract = method_signature + '<br>' +description
                    url = data_element.get('url').encode('utf-8')
                    list_of_data = [
                        name,                       # unique name
                        'A',                        # type is article
                        '',                         # no redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        'http://sass-lang.com/',    # add an external link back to SASS home
                        '',                         # no disambiguation
                        '',                         # images
                        abstract,                   # abstract
                        url                         # url to doc
                    ]
                    print list_of_data
                    line = '\t'.join(list_of_data)
                    output_file.write(line+'\n')

if __name__ == "__main__":
    file_path = 'download/Functions.html'
    data = PythonData(file_path)
    parser = PythonDataParser(data)
    parser.parse_for_data()
    output = PythonDataOutput(parser.get_data())
    output.create_file()
