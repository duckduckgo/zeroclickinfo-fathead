from bs4 import BeautifulSoup

SASS_DOC_BASE_URL = 'http://sass-lang.com/documentation/file.SASS_REFERENCE.html'
DOWNLOADED_HTML_PATH = 'download/file.SASS_REFERENCE.html'


class Data(object):
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


class DataParser(object):
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

        self.soup_data = BeautifulSoup(data_object.get_raw_data(), 'html.parser')
        table_of_contents = self.soup_data.find(class_="maruku_toc")
        sections = table_of_contents.find_all('li')
        for section in sections:
                section_id = section.find('a')
                section_id = section_id['href']
                heading = self.soup_data.find(id=section_id[1:])
                self.function_sections.append(heading)

                                          
    def parse_for_name(self, section):
        """
        Returns the section name
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            Name of section

        """
        name = section.text
        name = name.replace(":", "")
        name = name.replace("@", "")
        name = name.replace("-", "")
        name = name.replace("(", "")
        name = name.replace(")", "")
        name = name.replace(".", "")
        return name
    
    def parse_for_id(self, section):
        """
        Returns the section id
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            id of section

        """
        return '#'+ section.get('id')

    def parse_for_description(self, section):
        """
        Returns the function description
        Fixes up some weird double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a function description

        Returns:
            Function description

        """
        next_para = section.find_next('p')
        description = str(next_para.text.encode('utf-8'))
        next_tag  = next_para.find_next_sibling()
        if next_tag.name=="pre" or next_tag.name=="code":
            text = str(next_tag.encode('utf-8'))
            text = '\\n'.join(text.split('\n'))
            description = description + text
        return description

    def create_url(self, id):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the python doc

        """
        return SASS_DOC_BASE_URL + id

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        names = []
        
        for function_section in self.function_sections:
            name = self.parse_for_name(function_section)
            if name:
                description = self.parse_for_description(function_section)
                id = self.parse_for_id(function_section)

                url = self.create_url(id)
                if name in names:
                    index = names.index(name)
                    data_elements = data[index]
                    data_elements['description'] += description
                else:
                    names.append(name)
                    data_elements = {
                        'name': name,
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


class DataOutput(object):
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
        function = data_element.get('function')

        dotted_name = '{}{}{}'.format(function, '.' if function  else '', function)
        spaced_name = '{} {}'.format(function, function)

        return dotted_name.strip(), spaced_name.strip()

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'a') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    description = data_element.get('description')
                    url = data_element.get('url').encode('utf-8')
                    name = data_element.get('name').encode('utf-8')
                    list_of_data = [
                        name,                       # unique name
                        'A',                        # type is article
                        '',                         # no redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        '',                         # external link 
                        '',                         # no disambiguation
                        '',                         # images
                        description,                   # abstract
                        url                         # url to doc
                    ]
                    line = '\t'.join(list_of_data)
                    output_file.write(line+'\n')

    



if __name__ == "__main__":
    file_path = 'download/file.SASS_REFERENCE.html'
    data = Data(file_path)
    parser = DataParser(data)
    parser.parse_for_data()
    output = DataOutput(parser.get_data())
    output.create_file()