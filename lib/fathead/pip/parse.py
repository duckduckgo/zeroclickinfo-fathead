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
        soup = BeautifulSoup(page_source)
        namestr = ''
        name = page[:-5].split('_')
        for n in name:
            namestr += n + "-"

        namestr = namestr[:-1]

        pip_show = soup.find("div",{"id":namestr})

        sections = pip_show.findAll("div",{"class":"section"})






        for section in sections:

            #check if any more section in this if not then continue with this one

            if(len(section.findAll("div",{"class":"section"})) != 0):
                continue;

            for child in section:
                try:

                    text = child.text.strip()
                    if(text not in hashtable):
                        hashtable[text] = 1
                    else:
                        continue
                    if(child.find('div','highlight')):

                        fin = ''

                        for t in text.split('...'):
                            fin += t.strip() + '<br>'

                        fini = ''

                        for t in fin.split('\n'):
                            fini += t.strip() + '<br>'

                        text = '<pre><code>' + fini +'</pre></code>'

                    elif(child.find('a',{"class":"toc-backref"})):

                        text = '<h4>'+text[:-1]+'</h4>'



                    for t in text.split('\n'):
                        finaltext += t.strip() + '<br>'

                except:
                    pass

        finaltext = '<div class="prog__container">' + finaltext + '</div>'

        finaltext = ' '.join(finaltext.split())





    usage = unicode(finaltext).encode("utf-8")
    command = ''

    for p in page[:-5].split('_'):
        command += p + ' '


    command = unicode(command).encode("utf-8")
    command_url = unicode(baseUrl + page).encode("utf-8")

    list_of_data = [
                command,                      # unique name
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

    outyo.write('{}\n'.format('\t'.join(list_of_data)))


outyo.close()
