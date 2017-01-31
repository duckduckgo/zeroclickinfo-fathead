#!/usr/bin/python
# -*- coding: utf-8 -*-

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

import operator
import lxml.etree, lxml.html


base_url = "https://www.cia.gov/library/publications/the-world-factbook/fields/"
output = "output.txt"

f = open(output, "w");

countries_list = {};

tree = lxml.html.parse("download/raw.dat").getroot();
tables_body = tree.find_class("text-box")[0][0].findall('tr')[3][0];

tables = tables_body.findall('table');

c = 0;
for table in tables:
    # We need to find the tables after fieldlisting and after the header
    if c < 3:
      c += 1
      continue

    # Parse the real data
    row = table.findall('tr')[1]
    cells = row.findall('td')
    country = cells[0].text_content().strip();
    languages = cells[1].text_content().strip().replace("\n", "<br>");
    url = base_url + cells[0][0].get('href');
        
    countries_list[country] = [languages, url];


for country, information in countries_list.iteritems():
    country = unicode(country).encode("utf-8");
    languages = unicode(information[0]).encode("utf-8");
    url = unicode(information[1]).encode("utf-8");
    f.write("\t".join([country,        # title
                    "",                # namespace
                    url,               # url
                    languages,         # description
                    "",                # synopsis
                    "",                # details
                    "",                # type
                    ""                 # lang
                   ])
           );
    f.write("\n");
f.close()
