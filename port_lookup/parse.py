#!/usr/bin/python
# -*- coding: utf-8 -*-

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

import lxml.etree, lxml.html
import re

editlink = re.compile("action=edit"); 
iswikipedia = re.compile("wikipedia");
url = "https://secure.wikimedia.org/wikipedia/en/wiki/List_of_TCP_and_UDP_port_numbers"
output = "output.txt"

f = open(output, "w");

ports_list = {};

def get_port_range(ports):
    '''Returns a list with start and end of the range'''
    ports = ports.encode("utf8");
    p = ports.replace("–", "-").split("-");
    if len(p) == 2:
        return [int(p[0]), int(p[1])]
    else:
        return [int(p[0]), int(p[0])]

def get_protocol_string(tcp, udp):
    '''TCP/UDP string for description'''
    if tcp == "TCP" and udp == "UDP":
        return tcp + "/" + udp + " - ";
    elif tcp == "TCP" or udp == "UDP":
        return tcp + udp + " - ";
    else:
        return "";


tree = lxml.html.parse("raw.dat").getroot()
tables = tree.find_class("wikitable sortable")
for table in tables:
    for row in table.findall('tr'):
        cells = row.findall('td')
        if len(cells) != 5:
            continue;

        ports = get_port_range(cells[0].text_content());
        is_port_range = False;
        if ports[0] != ports[1]:
            is_port_range = True;
        protocol = get_protocol_string(cells[1].text_content(), cells[2].text_content());

        try:
            links = cells[3].findall('a');
        except:
            links = [];
        
        if len(links):
            for i in links:
                if not editlink.search(i.attrib['href']) and iswikipedia.search(i.attrib['href']):
                    # Convert link to Wikipedia format
                    i.text = "[[" + i.attrib["title"] + "|" + i.text_content() + "]]"

        # Remove citenote text
        description = re.sub("\[\d*\]", "", cells[3].text_content());
        # And [citation needed] text too
        description = re.sub("\[citation needed\]", "", description);

        status = cells[4].text_content();

        description = protocol + description + " (" + status + ")";
        if is_port_range:
            description += " [" + str(ports[0]) + "-" + str(ports[1]) + "]";

        for j in xrange(ports[0], ports[1] + 1):
            # Loop through the port range, and add to list or create list as necessary
            if ports_list.has_key(j):
                ports_list[j].append(description);
            else:
                ports_list[j] = [description];


for port, descriptions in ports_list.iteritems():
    description = unicode("<br />".join(descriptions)).encode("utf-8");
    f.write("\t".join([str(port),      # title
                    "",                # namespace
                    url,               # url
                    description,       # description
                    "",                # synopsis
                    "",                # details
                    "",                # type
                    ""                 # lang
                   ])
           );
    f.write("\n");
f.close()
