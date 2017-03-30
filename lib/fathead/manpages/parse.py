"""
Parses the downloaded pages.

Takes about 6 minutes to parse 9k+ pages. 
TODO: Use multiprocessing to speed up
"""

import os
import codecs
from bs4 import BeautifulSoup
import re
from tqdm import tqdm

OUT_FILE = 'output.txt'
abstract_fmt = '<section clas="prog__container"><p>{}</p>{}</section>'

title_regex = re.compile(r'(.*)\..*.html')

def escape_newlines(text):
    """Escape newlines in code."""
    return text.replace('\n', '\\n')


def extract_section(page, html):
    """
    Extract the description and synopsis (code) from the page.
    
    Too many try excepts because the format of some pages are different :(
    """
    try:
        synopsis = html.find('a', id='SYNOPSIS').parent.next_sibling.text
    except AttributeError:
        synopsis = ''
    
    try:
        description = html.find('a', id=re.compile(r'DESCRIPTION|OVERVIEW'))
        description = description..parent.next_sibling.text
    except AttributeError:
        try:
            # extract description from the name section using regex
            regex = '^.+\s-\s(.+)'
            name = html.find('a', id='NAME').parent.next_sibling.text
            description = re.match(regex, name.strip()).group(1)
            
        except AttributeError:  
            # fails because of groff_me, csysdig
            description = html.find(
                'b', text=re.compile('DESCRIPTION|DESCRIPTION SH')
            )
            
            if not description:
                try:
                    description = html.find('a', id='RETURN_VALUE')
                    description = description..parent.next_sibling
                except AttributeError:
                    # then it's Yum use the synopsis as the description
                    description = synopsis
                    
                    return description, ''
            
            description = description.text
                
    return description, synopsis

def parse_html(page):
    """Extract useful bits of data from the pages."""
    
    page_location = os.path.join('download', page)
    
    with codecs.open(page_location, 'rb', encoding='UTF-8') as infile:
        html = BeautifulSoup(infile, 'html.parser')
        
        # Extract title from filename
        title = re.match(title_regex, page).group(1)
        
        # extract description and code
        description, code = extract_section(page, html)
        
        # quicker than using a regex to replace whitespace and new lines
        description = ' '.join(description.split())
        
        # clean up and prepare code section
        code.strip()
        code = escape_newlines(code)
        code = '<pre><code>{}</code></pre>'.format(code) if code else code
        
        # generate abstract
        abstract = abstract_fmt.format(description, code)
        
        url = 'http://man7.org/{}'.format(page)
        
        return [title, 'A', '', '', '', '', '', '', '', '', '', abstract, url]
        

def main():
    """Main method."""
    with codecs.open(OUT_FILE, 'wb', encoding='UTF-8') as outfile:
        for page in tqdm(os.listdir('download'), ascii=True, desc='parsing:'):
            out_data = parse_html(page)
            out_data = '\t'.join(out_data)
            
            outfile.write(out_data)
            outfile.write('\n')

if __name__ == '__main__':
    main()