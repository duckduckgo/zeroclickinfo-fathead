#!/usr/bin/python
# -*- coding: utf-8 -*-

import lxml.etree, lxml.html
import re

url = "http://redis.io"
output = "output.txt"

f = open(output, "w");

tree = lxml.html.parse("download/raw.dat").getroot()
commands = tree.find_class("command")

data = {}

for command in commands:

    for row in command.findall('a'):
        command_url = "%s%s" % (url, row.get('href'))
        
        for sibling in command.itersiblings():
            usage = ""
            
            for command_args in command.findall('span'):
                usage = "%s %s" % (row.text, command_args.text.replace(' ', '').replace('\n', ' ').strip())

            summary = "%s." % (re.sub('\.$', '', sibling.text))

            data[command_url] = (row.text, summary, usage)

for command_url in data.keys():
    command, summary, usage = data[command_url]
    summary = unicode(summary).encode("utf-8")
    usage = unicode(summary).encode("utf-8")
    
    f.write("\t".join([str(command),      # title
                    "",                # namespace
                    command_url,               # url
                    summary,       # description
                    usage,                # synopsis
                    "",                # details
                    "",                # type
                    ""                 # lang
                   ])
           )
    f.write("\n")
f.close()
