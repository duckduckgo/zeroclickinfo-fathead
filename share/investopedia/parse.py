#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import json

jsonFile = "./download/terms.json"
outputFile = "./output.txt"
outputStr = ""


def formatLine (term):
    descr = "{0}<br>{1}".format(term["definition"], term["investopediaSays"])
    descr = descr.replace('\\n', '~~~n')
    descr = descr.replace('\n', '\\n')
    descr = descr.replace('~~~n', '\\\\n')
    descr = descr.replace('\t', '\\t')
    tmp = [
        "%s" % term["title"],   #title
        "D",                    #type
        "",                     #redirects
        "",                     #otheruses
        "financy_terms",        #categories
        "",                     #references
        "",                     #see_alse
        "",                     #further_reading
        "",                     #external_links
        "",                     #disambiguation
        "",                     #images
        "%s" % descr,           #abstract/description
        "%s" % term["link"],    #source_url
        ]
    return "\t".join(tmp) + "\n"


def saveOutput (outputStr):
    fh = open(outputFile, "wt")
    fh.write(outputStr)
    fh.close


termArr = json.load(open(jsonFile))
for term in termArr:
    #print(term)
    #print(formatLine(term))
    outputStr += formatLine(term)

saveOutput(outputStr)
