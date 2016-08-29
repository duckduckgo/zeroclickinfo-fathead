# -*- coding: utf-8 -*-
import os

from bs4 import BeautifulSoup

SASS_DOC_BASE_URL = 'http://sass-lang.com/documentation/Sass/Script/Functions.html'
DOWNLOADED_HTML_PATH = 'download/Functions.html'


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
        sections = self.soup_data.find_all('dl', {'class': None})
        for section in sections:
            function_names = section.find_all('dt')
            function_descriptions = section.find_all('dd')
            for i in range(len(function_names)):
                self.function_sections.append([function_names[i], function_descriptions[i]])
        """
            functions = section.find_all('dl', {'class': 'function'})
            if functions:
                self.function_sections.extend(functions)
        """

    def parse_for_module_name(self, section):
        """
        Returns the module name
        Args:
            section: A section of parsed HTML that represents a function definition

        Returns:
            Name of the module

        """
        module_name = section.find('a')
        if module_name:
            #return the module name without paramaters
            return module_name.text.split('(')[0] 
            
        return ''
                                          
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
    def parse_for_section_div(self, section):
        """
        Return a div containing more information about the section function
        Args:
            section: A section of parsed HTML that represents a function definition
        Returns:
            A div element
        """
        anchor = self.parse_for_anchor(section)
        heading = self.soup_data.find(id=anchor[1:])
        return heading.parent

    def parse_for_example(self, section):
        info = self.parse_for_section_div(section)
        example = info.find('div', {'class': 'examples'})
        if example:
            code = example.find('pre')
            text = '<h5>Examples</h5>'+ str(code)
            text = '\\n'.join(text.split('\n'))
            return text
        return None
    
    def parse_for_parameters(self, section):
        info = self.parse_for_section_div(section)
        parameters = info.find('ul', {'class': 'param'})
        if parameters:
            code = self.fix_parameter_links(parameters)
            text = '<h5>Parameters</h5><ul>'
            code = code.find_all('li')
            for parameter in code:
                text = text + '<li>'
                name = parameter.find('span', {'class':"name"})
                if name:
                    text = text + name.text
                inline = parameter.find('div', {'class':"inline"})
                if inline:
                    inline = parameter.find('p')
                    inline = str(inline)
                    inline = inline.replace('’','&#39;')
                    inline = inline.strip('<p>')
                    inline = inline.strip('</p>')
                    text = text + " - " + inline
                text = text + '</li>'
            text = text + '</ul>'
        
            return text
        return None
        
    def fix_parameter_links(self, parameters):
        for a in parameters.findAll('a'):
            path = a['href']
            a['href'] = a['href'].replace(a['href'],'http://sass-lang.com/documentation/Sass/Script/'+path)
        return parameters
    def create_url(self, anchor):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the sass doc

        """
        return SASS_DOC_BASE_URL + anchor

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        functions = []
        
        for function_section in self.function_sections:
            function = self.parse_for_function_name(function_section[0])
            if function:
                method_signature = self.parse_for_method_signature(function_section[0])
                description = self.parse_for_description(function_section[1])
                anchor = self.parse_for_anchor(function_section[0])
                example = self.parse_for_example(function_section[0])
                parameter = self.parse_for_parameters(function_section[0])
                abstract = description + "<br>" + method_signature
                if example:
                    abstract =  abstract + example
                if parameter:
                     abstract = "%s<br>%s"%(abstract, parameter)

                url = self.create_url(anchor)
                if function in functions:
                    index = functions.index(function)
                    data_elements = data[index]
                    data_elements['abstract'] += abstract
                else:
                    functions.append(function)
                    data_elements = {
                        'function': function,
                        'abstract': abstract,
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
                if data_element.get('function'):
                    name = data_element.get('function').encode('utf-8')
                    abstract = data_element.get('abstract').encode('utf-8')
                    url = data_element.get('url').encode('utf-8')
                    
                    list_of_data = [
                        name,                       # unique name
                        'A',                        # type is article
                        '',                         # redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        '',                         # external link
                        '',                         # no disambiguation
                        '',                         # images
                        abstract,                   # abstract
                        url                         # url to doc
                    ]
                    
                    line = '\t'.join(list_of_data)
                    
                    output_file.write(line+'\n')
    def create_redirect(self):
        """
        Iterate through the data and add redirects to output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'a') as output_file:
            for data_element in self.data:
                if data_element.get('function'):
                    name = data_element.get('function').encode('utf-8')
                    abstract = data_element.get('abstract').encode('utf-8')
                    url = data_element.get('url').encode('utf-8')
                    
                    list_of_data = [
                        name + ' function',         # unique name
                        'R',                        # type is article
                        name,                       # redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        '',                         # external link
                        '',                         # no disambiguation
                        '',                         # images
                        '',                         # abstract
                        ''                          # url to doc
                    ]
                    
                    line = '\t'.join(list_of_data)
                    
                    output_file.write(line+'\n')

if __name__ == "__main__":
    file_path = 'download/Functions.html'
    data = Data(file_path)
    parser = DataParser(data)
    parser.parse_for_data()
    output = DataOutput(parser.get_data())
    output.create_file()
    output.create_redirect()