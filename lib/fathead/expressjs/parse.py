#!/usr/bin/env python3.5
# * encoding utf-8 *
# * Parsing downloads/docs.html *

import re
from bs4 import BeautifulSoup

__MODULE__ = "EXPRESS_JS"
__AUTHOR__ = "adityatandon007@DDG_COMMUNITY"

INFILE = "downloads/docs.html"
OUTFILE = "output.txt"


abstract_fmt = '<section class="prog__container"><p>{}</p>{}</section>'
url_fmt = 'http://expressjs.com/en/api.html'


def clean_para(para):
    """ Remove newlines from paragraph """
    return re.sub('\r?\n+', '', para)
 
def clean_code(code):
    """ Escape newlines and tabs """
    return code.replace('\n', '\\n').replace('\t', '    ')

def get_paragraphs(parsing_doc):
    
    """ Parsing main headings """
    
    h2s = parsing_doc.find_all("h2")
    for h2 in h2s:
        """ Generating titles and urls for main headings """  
        title = h2.get("id")
        url_link = url_fmt + "#" + title
        
        """ Generating paragraph """
        p = h2.find_next_sibling(text=None)
        if p.name == "p":
            para = clean_para(p.text)    
        else:
            para = clean_para(p.p.text)
            
        """ Generating code if any """
        code = h2.find_next("pre")
        code = c
        #    code = 
        #    abstract = abstract_fmt.format(para,code)

    sections = parsing_doc.find_all("section")
    for sect in sections:
        h3 = sect.find("h3")
        if h3:
            url_link = url_fmt + "#" + h3.get("id")
            print(url_link)
            p = h3.find_next_sibling(text=None)
            if p.name == "p":
                para = p.text
                print(para)
        
            code = h3.find_next("pre")
            print(code)
    
         
        
def main():
    """ Creating the soup """
    
    with open(INFILE, "r", encoding="utf-8") as f, open(OUTFILE, "w", encoding="utf-8") as o:
        
        soup = BeautifulSoup(f, "html.parser")

        parsing_doc = soup.find("div", {"id": "api-doc"})

        get_paragraphs(parsing_doc)
    

    
if __name__ == "__main__":
    main()
    