#!/usr/bin/python
# -*- coding: utf-8 -*-

import lxml.etree, lxml.html
import re

url = "http://redis.io"
output = "output.txt"

f = open(output, "w")

tree = lxml.html.parse("download/raw.dat").getroot()

"""
Extract all <li> elements
First 8 elements are website's meta links, so ignore them
"""
elements = tree.findall(".//li")[8:]

data = {}

# Iterate over all desired <li> elements
for element in elements:

    # Find <a> tag within this <li> element
    link = element.find('a')
    # Save `href` attribute for this <a> tag
    command_url = "{}{}".format(url, link.get('href'))

    # Find an element with class='command'
    for command in link.find_class('command'):
        # Save command name
        command_text = command.text.strip()

        # Find an element with class='args'
        for span in command.find_class('args'):
            span_text = span.text.replace(' ', '').replace('\t', '').replace('\n', ' ').strip()
            if len(span_text) > 0:
                # Save command usage
                command_usage = "{}{}".format(command_text, span_text)

    # Find an element with class='summary'
    for summary in link.find_class('summary'):
        # Save command summary (description)
        command_summary = "{}.".format(summary.text.strip())

    data[command_url] = (command_text, command_summary, command_usage)

for command_url in sorted(data.keys()):
    command, summary, usage = data[command_url]
    summary = unicode(summary).encode("utf-8")
    usage = unicode(usage).encode("utf-8")

    f.write("\t".join([str(command),      # title
                    "",                   # namespace
                    command_url,          # url
                    summary,              # description
                    usage,                # synopsis
                    "",                   # details
                    "",                   # type
                    ""                    # lang
                   ])
           )
    f.write("\n")
f.close()
