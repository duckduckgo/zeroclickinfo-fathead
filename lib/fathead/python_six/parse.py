from bs4 import BeautifulSoup
import glob 


class Command(object):
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
        usage_cleaned = self.usage.replace('\n', '\\n').replace('\t', '    ')

        # Make the abstract have the description as well as a code block
        # for the usage of the command
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
