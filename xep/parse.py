#!/usr/bin/python
# -*- coding: utf-8 -*-

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

from glob import glob
from lxml import etree

OUTPUT = "output.txt"

out = open(OUTPUT, "w")
xeps = {}


files = glob('download/extensions/xep-????.xml')
for f in files:
    tree = etree.parse(f)

    title = tree.xpath('/xep/header/title')[0].text
    number = 'XEP-' + tree.xpath('/xep/header/number')[0].text
    url = 'http://xmpp.org/extensions/' + number.lower() + '.html'
    abstract = tree.xpath('/xep/header/abstract')[0].text
    image = '';
    
    # The ZCI box looks weird if there is an image with a small abstract
    if len(abstract) > 150:
        image = '[[Image:http://xmpp.org/images/xmpp.png]]'

    abstract = abstract.replace('\n', ' ')
    abstract = abstract.replace('\t', ' ')
    
    out.write('\t'.join([number, "A", "", "", "", "", "", "", "", "",
                         image,
                         abstract,
                         url]) + '\n')
