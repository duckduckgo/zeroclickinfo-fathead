#!/usr/bin/env python3

from bs4 import BeautifulSoup

baseUrl = 'https://pip.pypa.io/en/stable/reference/'

output = "output.txt"

outyo = open(output, "w");

import os
from os import path
pages = [f for f in os.listdir('./download/')]

out = {}

for page in pages:

    finaltext = ''
    hashtable = {}
    with open('./download/' + page, 'r') as content_file:
        page_source = content_file.read()
        soup = BeautifulSoup(page_source,"lxml")
        namestr = ''
        name = page[:-5].split('_')
        for n in name:
            namestr += n + "-"

        namestr = namestr[:-1]

        pip_show = soup.find("div",{"id":namestr})

        description = pip_show.findAll("div",{"class":"section","id":"description"})
        usages = pip_show.findAll("div",{"class":"section","id":"usage"})

        for usage in usages:
            for child in usage:
                try:
                    text = child.text.strip()
                    if(text not in hashtable):
                        hashtable[text] = 1
                    else:
                        continue
                    if(child.find('div','highlight')):
                        fin = ''

                        for t in text.split('...'):
                            fin += t.strip() + '\n'

                        fini = ''

                        for t in fin.split('\n'):
                            fini += t.strip() + '\n'

                        text = '<pre><code>' + fini +'</code></pre>'

                    elif(child.find('a',{"class":"toc-backref"})):
                        text = '<h4>'+text[:-1]+'</h4>'

                    for t in text.split('\n'):
                        if(t.strip() == '' and finaltext[-4:] == '<br>'):
                            continue;
                        finaltext += t.strip() + '<br>'

                except:
                    pass

        for section in description:

            flag = 0
            for child in section:
                try:

                    text = child.text.strip()
                    if(text not in hashtable):
                        hashtable[text] = 1
                    else:
                        continue
                    if(child.find('div','highlight')):
                        break;

                    elif(child.find('a',{"class":"toc-backref"})):
                        if(text[:-1] != 'Description'):
                            break;
                        text = '<h4>'+text[:-1]+'</h4>'

                    for t in text.split('\n'):
                        if(t.strip() == '' and finaltext[-4:] == '<br>'):
                            continue;
                        finaltext += t.strip() + '<br>'
                except:
                    pass
                
        if(page == 'pip.html'):
            finaltext += 'pip is a package management system used to install and manage software packages written in Python. Many packages can be found in the Python Package Index (PyPI). <br>'
        
        finaltext = '<section class="prog__container">' + finaltext + '</section>'

        finaltext = ' '.join(finaltext.split())
        finaltext = finaltext.replace('<br>','\\n')




    usage = finaltext
    command = ''

    for p in page[:-5].split('_'):
        command += p + ' '



    command_url = baseUrl + page

    list_of_data = [
                command,                    # unique name
                'A',                        # type is article
                '',                         # no redirect data
                '',                         # ignore
                '',                         # no categories
                '',                         # ignore
                '',                         # no related topics
                '',                         # ignore
                baseUrl,                   # add an external link back to Scikit home
                '',                         # no disambiguation
                '',                         # images
                usage,                   # abstract
                command_url                      # url to doc
            ]

    final = ''
    for l in list_of_data:
        final = final + l + '\t'

    final = final + '\n'
    outyo.write(final)


outyo.close()
