__author__ = 'kchahal'
#To change this template use Tools | Templates.

from bs4 import BeautifulSoup

baseUrl = 'https://pip.pypa.io/en/stable/reference/'

output = "output.txt"

outyo = open(output, "w");

import os
from os import path
pages = [f for f in os.listdir('./download/')]

out = {}
print pages
for page_links in pages:
    with open('./download/' + page_links, 'r') as content_file:
        page_source = content_file.read()

        soup = BeautifulSoup(page_source)
        mydivs = soup.findAll("div", { "class" : "highlight-python" })
        #print mydivs[0].text

        value = {}
        value['usage'] = str(mydivs[0].text).strip()

        descrip = soup.find("div",{ "id" : "description" })
        #print descrip.text
        value['command_url'] =  baseUrl + page_links
        value['description'] = descrip.text.strip()

        out[page_links] = value
#print out

for value in out.iteritems():
    print('Item is {} \n'.format(str(value[0])))

    print('Usage is {} \n'.format(str(value[1]['usage'])))
    #print('Usage is %s \n',value[1][1])
    usage = unicode('<br><pre><code>{}</code></pre>'.format(value[1]['usage'])).encode("utf-8")
    command = unicode(value[0][:-5]).encode("utf-8")
    command_url = unicode(value[1]['command_url']).encode("utf-8")

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
