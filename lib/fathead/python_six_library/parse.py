# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
import glob
import sys

python_six_base_url = 'https://pythonhosted.org/six'


class SixModule(object):
    """
    Class for storing all necessary information about the python six lib. This
    information is then used to create the output.txt file using the
    __str__() method on this class
    """

    def __init__(self, name, description, filename, is_function):

        self.name = name
        self.description = description.replace('\n', '\\n').replace('\t', '    ')
        self.description = '<p>{}</p>'.format(self.description)
        self.filename = filename
        self.is_function = is_function
        self.usage = ''

    def __str__(self):
        """
        Output the python six information in the proper format required for
        DuckDuckGo Fatheads
        """

        # Clean up the usage statement which can have newline characters and
        # tab characters, which mess up how it renders 
        code = '<pre><code>{}</code></pre>'.format(
            self.usage.replace('\n', '\\n').replace('\t', '    ')) if self.usage else ''

        # Make the abstract have the description as well as a code block
        if code:
            abstract = '{}{}'.format(self.description, code)
        else:
            abstract = self.description
        abstract = '<section class="prog__container">{}</section>'.format(abstract)
        return '\t'.join([
            self.name,  # Full article title
            'A',  # Type of article
            '',  # For redirects only
            '',  # Ignore
            '',  # Categories
            '',  # Ignore
            '',  # Related Topis
            '',  # Ignore
            '',  # External links
            '',  # For disambiguation pages only
            '',  # Image
            abstract,  # Abstract
            '{}/{}'.format(python_six_base_url,
                           self.filename),  # URL
        ])


class Parser(object):
    def __init__(self):
        """Get all files that need to be parsed"""
        self.files_to_parse = glob.glob('download/*.html')
        self.six_module = []

    def parse_module(self):
        """Parse module and make a SixModule object for each"""
        self.six_module = []

        for file in self.files_to_parse:
            soup = BeautifulSoup(open(file), 'html.parser')
            div = soup.find('div', {'class': 'section'})

            if not div:
                continue

            for tag in div.find_all('dl'):
                '''Get the module name '''
                if tag.dt.select('code')[1].get_text(strip=True) != 'six.':
                    module_name = tag.dt.select('code')[1].get_text(strip=True)
                else:
                    module_name = tag.dt.select('code')[2].get_text(strip=True)

                '''Get the module desc '''
                description = tag.dd.p.getText()

                '''Get code for module if present '''
                code = tag.dd.div.getText() if tag.dd.div else None

                '''Determine if the entry is a function or a field'''
                is_function = tag.get('class')[0] == u'function'

                # Create the SixModule object
                module = SixModule(module_name, description, file.replace('download/', ''), is_function)
                module.usage = code
                # Add the module in SixModule
                self.six_module.append(module)


if __name__ == '__main__':
    reload(sys)
    sys.setdefaultencoding('utf-8')
    # Create the Parser
    parser = Parser()

    parser.parse_module()

    # Write the output in Output.txt
    with open('output.txt', 'wb') as output, open('coverage/functions.txt', 'wb') as functions:
        for module in parser.six_module:
            output.write((module.__str__() + '\n'))
            if module.is_function:
                functions.write((module.name + '\n'))
