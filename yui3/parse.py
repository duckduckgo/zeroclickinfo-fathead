#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
from BeautifulSoup import BeautifulSoup
from collections import defaultdict
# We use defaultdict to append duplicates, but output only the first entry

modules = defaultdict(list)

def normalize(string):
    ''' -> Remove parantheses endings from module names 
        -> Remove YUI from module name
    '''
    return re.sub('( ?\(.*\)$|YUI\ ?[2-3]?[ -]?)', '', string)

# Parse the official modules
official_soup = BeautifulSoup(open('data/official.html'))
for module in official_soup.findAll('li', {'class' : 'component'}):
    modules[module.a.text].append( dict(link = module.a['href'], 
                                        name = module.a.text,
                                        descr = module.a['data-tooltip'] + '<br />(This is an official module)') 
                                 )
# Parse the community supported gallery modules
gallery_soup = BeautifulSoup(open('data/gallery.html'))
for module in gallery_soup.findAll('a', href = re.compile('/gallery/show/.+')):
    if 'patch' in module.text.lower():
        continue
    h4 = module.findNext('h4')
    if h4.span:
        descr = h4.span.next.next + '<br />(This is a gallery module, available on CDN)'
    else:
        descr = h4.next.next + '<br />(This is a gallery module)'

    modules[normalize(module.text)].append( dict(link = module['href'], 
                                                 descr = descr, 
                                                 name = module.text) )

f = open('output.txt', 'w');
for name, value in modules.items():
    f.write('\t'.join([name,      
                        '',             
                        'http://yuilibrary.com' + value[0]['link'],            
                        value[0]['descr'].replace('\n', '<br />'), 
                        '',             
                        '',             
                        '',             
                        ''              
                       ]) + "\n")
f.close()
