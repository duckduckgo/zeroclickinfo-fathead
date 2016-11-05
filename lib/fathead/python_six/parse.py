# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
import glob
import sys

python_six_base_url = 'https://pythonhosted.org/six/'

class SixModule(object):
    '''
    Class for storing all necessary information about the python six lib. This
    information is then used to create the output.txt file using the
    __str__() method on this class
    '''
    def __init__(self, name, description, filename):
        
        self.name = name
        self.description = description
        self.filename = filename
        self.usage = ''

    def __str__(self):
        '''
        Output the python six information in the proper format required for
        DuckDuckGo Fatheads
        '''

        # Clean up the usage statement which can have newline characters and
        # tab characters, which mess up how it renders
#        print self.usage

        usage_cleaned = self.usage.replace("’","").replace("\t", "    ").replace("\n","\\n")
        
#        print usage_cleaned

        # Make the abstract have the description as well as a code block
        
        abstract = '{}\n<pre><code>{}</pre></code>'.format(self.description,
                                                           usage_cleaned)
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
        '''Get all files that need to be parsed'''
        self.files_to_parse = glob.glob('download/*.html')

    def parse_module(self):
        '''Parse module and make a SixModule object for each'''
        self.six_module = []

        for file in self.files_to_parse:
            soup = BeautifulSoup(open(file), 'html.parser')
            div = soup.find('div',{'class':'section'})
        
            if not div :
                continue
        
            for tag in div.find_all('dl'):
            
                '''Get the module name '''
                module_name = tag.dt.select('code')[1].get_text(strip=True) if tag.dt.select('code')[1].get_text(strip=True) != 'six.' else tag.dt.select('code')[2].get_text(strip=True)
            
                '''Get the module desc '''
                description = tag.dd.p.getText()
            
                '''Get code for module if present '''
                code = tag.dd.div.getText() if tag.dd.div else ''

                # Create the SixModule object
                module = SixModule(module_name, description,file.replace('download/', ''))
                module.usage = code
                #Add the module in SixModule
                self.six_module.append(module)

if __name__ == '__main__':

    reload(sys)
    sys.setdefaultencoding('utf-8')
    #Create the Parser
    parser = Parser()

    parser.parse_module()

    # Write the output in Output.txt
    with open('output.txt', 'w+') as output:
        for module in parser.six_module:
            output.write((module.__str__() + '\n'))
