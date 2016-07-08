import os

from bs4 import BeautifulSoup

PYTHON_DOC_BASE_URL = 'https://docs.python.org/3.4/{}'
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


class PythonDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains Python data
    """
    def __init__(self, raw_data):
        """
        Given raw data, get the relevant sections
        Args:
            raw_data: HTML data
        """
        self.parsed_data = None
        self.function_sections = []

        soup_data = BeautifulSoup(raw_data, 'html.parser')
        sections = soup_data.find_all('div', {'class': 'section'})
        
        for section in sections:
            functions = section.find_all('dl', {'class': 'function'})
            if functions:
                self.function_sections.extend(functions)

    def parse_for_module_name(self, section):
        module_name = section.find('code', {'class': 'descclassname'})
        print(module_name)
        if module_name:
            return module_name.rstrip('.')
        return ''
                                          
    def parse_for_function_name(self, section):
        pass

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        
        for function_section in self.function_sections:
            module = self.parse_for_module_name(function_section)
            function = self.parse_for_function_name(function_section)
            
            data_elements = {
                'module': module,
                'function': function
            }

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
        with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    name = ''
                    abstract = ''
                    url = ''
                    list_of_data = [
                        name,       # unique name
                        'A',        # type is article
                        '',         # no redirect data
                        '',         # ignore
                        '',         # no categories
                        '',         # ignore
                        '',         # no related topics
                        '',         # ignore
                        '',         # add an external link back to Python home
                        '',         # no disambiguation
                        '',         # images
                        abstract,   # abstract
                        url         # url to doc
                    ]
                    output_file.write('{}\n'.format('\t'.join(list_of_data)))


if __name__ == "__main__":
    for dir_path, dir_name, file_names in os.walk(DOWNLOADED_HTML_PATH):
        for file_name in file_names:
            if '.html' in file_name:
                data = PythonData('/'.join((dir_path, file_name)))
                parser = PythonDataParser(data.get_raw_data())
                parser.parse_for_data()
#                 output = PythonDataOutput(parser.get_data())
#                 output.create_file()
