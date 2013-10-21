#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
from BeautifulSoup import BeautifulSoup
from collections import defaultdict
# Use defaultdict to append duplicates, but output only the first entry

modules = defaultdict(list)


def normalize(string):
    ''' -> Remove parantheses part from ending of module names
        -> Remove YUI from module name
    '''
    return re.sub('( ?\(.*\)$|YUI\ ?[2-3]?[ -]?)', '', string)


def get_descr_string(type, descr):
    return '''<i>Type</i>: %s<br /><i>Description</i>: %s''' \
        % (type, re.sub('(\n|\r)', '<br />', descr))

# Parse the official modules
official_soup = BeautifulSoup(open('data/official.html'))
for module in official_soup.findAll('li', {'class': 'component'}):
    mah = module.a['href']
    descr = get_descr_string('Official', module.a['data-tooltip'])
    modules[module.a.text].append({'link': 'http://yuilibrary.com%s' % mah,
                                   'name': module.a.text,
                                   'descr': descr
                                   })

# Parse the community supported gallery modules
gallery_soup = BeautifulSoup(open('data/gallery.html'))
for module in gallery_soup.findAll('a', href=re.compile('/gallery/show/.+')):
    if 'patch' in module.text.lower():
        continue
    h4 = module.findNext('h4')
    if h4.span:
        hsnn = h4.span.next.next
        descr = get_descr_string('Gallery, available on CDN', hsnn)
    else:
        descr = get_descr_string('Gallery', h4.next.next)
    mh = module['href']
    mt = normalize(module.text)
    modules[mt].append({'link': 'http://yuilibrary.com%s' % mh,
                        'descr': descr,
                        'name': module.text})

with open('output.txt', 'w') as f:
    for name, value in modules.items():
        f.write('\t'.join(
                [
                    name,  # title
                    'A',   # type
                    '',    # redirect
                    '',    # otheruses
                    '',    # categories
                    '',    # references
                    '',    # see_also
                    '',    # further_reading
                    '',    # external_links
                    '',    # disambiguation
                    '',    # images
                    value[0]['descr'],   # abstract
                    value[0]['link']     # source_url
                    ]
                ) + "\n")
