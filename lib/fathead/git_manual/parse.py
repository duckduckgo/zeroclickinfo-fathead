# -*- coding: utf-8 -*-                                                                                                                    

from bs4 import BeautifulSoup

import glob

git_docs_base_url = open('data.url').read().strip()


class Command(object):
    ''' 
    Class for storing all necessary information about the git command. This
    information is then used to create the output.txt file using the
    __str__() method on this class
    '''
    def __init__(self, name, description, filename):
        '''Instantiate the information about the command'''
        self.name = name
        self.description = description
        self.filename = filename
        self.usage = ''

    def __str__(self):
        ''' 
        Output the git command information in the proper format required for
        DuckDuckGo Fatheads
        '''

        # Make the abstract have the description as well as a code block
        # for the usage of the command
        abstract = '{}\n<pre><code>{}</pre></code>'.format(self.description,
                                                           self.usage.replace('\n', '\\n'))
        return '\t'.join([
            self.name, # Full article title
            'A', # Type of article
            '', # For redirects only
            '', # Ignore
            '', # Categories
            '', # Ignore
            '', # Related Topis
            '', # Ignore
            '', # External links
            '', # For disambiguation pages only
            '', # Image
            abstract, # Abstract
            '{}/{}'.format(git_docs_base_url,
                           self.filename), # URL
        ])

class Parser(object):
    def __init__(self):
        '''Get all git command files that need to be parsed'''
        self.files_to_parse = glob.glob('download/*.html')

    def parse_commands(self):
        '''Parse each git command and make a Command object for each'''
        self.commands = []

        for file in self.files_to_parse:
            soup = BeautifulSoup(open(file), 'html.parser')

            # The '_name' H2 element always precedes the name and description
            # of the command
            name_h2 = soup.find('h2', {'id': '_name'})

            if not name_h2:
                continue

            # The next <p> element has the name and description in it
            description_p = name_h2.findNext('p')

            if not description_p:
                continue

            description = description_p.getText()

            # Split out the command name and short description of the command                                                              
            (command_name, description) = description.split(" - ")
            
            # Create the Command object
            command = Command(command_name, description, file.replace('download/', ''))

            # Now find the '_synopsis' H2 element which will give us the usage
            synopsis_h2 = soup.find('h2', {'id': '_synopsis'})

            if synopsis_h2:
                # The next <pre> element has the usage in it
                usage_pre = synopsis_h2.findNext('pre')

                if usage_pre:
                    usage = usage_pre.getText()

                    # Add the usage to the Command instance
                    command.usage = usage

            # Add the command to the list of commands
            self.commands.append(command)

if __name__ == '__main__':
    # Create the parser
    parser = Parser()

    # Parse the commands
    parser.parse_commands()

    # Write the output for each command into the output.txt file
    with open('output.txt', 'wb') as output:
        for command in parser.commands:
            output.write((command.__str__() + '\n').encode('utf-8'))