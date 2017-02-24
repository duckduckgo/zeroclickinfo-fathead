#!/usr/bin/env python3.5
# _*_ encoding: utf-8 _*_
# * Parsing downloads/docs.html *

import re
from bs4 import BeautifulSoup

__MODULE__ = 'EXPRESS_JS FATHEAD'
__AUTHOR__ = 'adityatandon007@DDG_COMMUNITY'

INFILE = 'downloads/docs.html'
OUTFILE = 'output.txt'

abstract_fmt = '<section class="prog__container"><p>{}</p>{}</section>'
url_fmt = 'http://expressjs.com/en/api.html'


def clean_para(para):
    """ Remove newlines from paragraph """
    return re.sub('\r?\n+', '', para)
 

def clean_code(code):
    """ Escape newlines and tabs """
    return code.replace('\n', '\\n').replace('\t', '    ')


""" Headings generator """

def get_headings(h2s):
    
    """ Parsing headings """
    
    for h2 in h2s:
    
        """ Generating titles and urls for headings """  
        
        title = h2.get('id')
        url = url_fmt + '#' + title
        
        """ Generating paragraph """
        
        p = h2.find_next_sibling(text= None)
        if p.name == 'p':
            para = clean_para(p.text)    
        else:
            para = clean_para(p.p.text)
            
        """ Generating code if any """
        
        pre = h2.find_next('pre')
        code = clean_code(pre.text)
        code = '<pre><code>{}</code></pre>'.format(code)
        
        """ Generating abstract for the headings """
        
        abstract = abstract_fmt.format(para, code)
        
        """ Generating output entry """
        
        yield [title, 'A', '', '', '', '', '', '', '', '', '', abstract, url]
            

""" Methods and Properties generator """
        
def get_methods_properties(sections):
    
    """ Parsing methods and properties """

    for sect in sections:
        h3 = sect.find('h3')
        if h3:
            
            """ Generating titles and urls for methods and properties """
            title = h3.get('id')
            url = url_fmt + '#' + h3.get('id')
            
            """ Generating paragraph """
            
            p = h3.find_next_sibling(text= None)
            if p.name == 'p':
                para = clean_para(p.text)
                
            """ Generating code if any """
            
            pre = h3.find_next('pre')
            code = clean_code(pre.text)
            code = '<pre><code>{}</code></pre>'.format(code)
            
            """ Generating abstract for the methods and properties """
            
            abstract = abstract_fmt.format(para, code)
            
            """ Generating output entry """
           
            yield [title, 'A', '', '', '', '', '', '', '', '', '', abstract, url]
         

        
def main():
    """ Creating the soup """
    
    with open(INFILE, 'r', encoding= 'utf-8') as f, open(OUTFILE, 'w', encoding= 'utf-8') as o:
          
        soup = BeautifulSoup(f, 'html.parser')
        parsing_doc = soup.find('div', {'id': 'api-doc'})
        
        """ Headings entry """
        
        h2s = parsing_doc.find_all('h2')
        for h2_entry in get_headings(h2s):
            output = '\t'.join(h2_entry)
            o.write(output + '\n')
            
        """ Methods and properties entry """
        
        sections = parsing_doc.find_all('section')
        for h3_entry in get_methods_properties(sections):
            output = '\t'.join(h3_entry)
            o.write(output + '\n')

    

if __name__ == '__main__':
    print('STARTING ' + __MODULE__)
    print('PARSING.....')
    main()
    print('PARSING COMPLETED')
