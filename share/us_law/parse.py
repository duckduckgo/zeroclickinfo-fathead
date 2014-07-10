#!/usr/bin/env python3

from bs4 import BeautifulSoup
import os

def find_parents(n, parents):
    parents.insert(0, (n.parent.name, n.parent.num, n.parent.heading))

    if n.parent.name == "title":    #top level
        return parents
    else:
        return find_parents(n.parent, parents)

for title in os.listdir("download/xml/"):
    soup = BeautifulSoup(open("download/xml/%s" % title))
    if soup.appendix:
        continue

    for section in soup.find_all("section"):
        if section.get("id") == None:
            continue    #not a leaf node

        chain = find_parents(section, []) + [(section.name, section.num, section.heading)]

        num_title = chain[0][1].get("value")
        num_sec = chain[-1][1].get("value")

        context = " ".join([x[1].get_text() + " " + x[2].get_text() for x in chain])
        url = "http://www.law.cornell.edu/uscode/text/%s/%s" % (num_title, num_sec)

        out = "%s USC %s\t" % (num_title, num_sec)        #0
        out += "A\t"        #1
        out += "\t"        #2
        out += "\t"        #3
        out += "\t"        #4
        out += "\t"        #5
        out += "\t"        #6
        out += "\t"        #7
        out += "\t"        #8
        out += "\t"        #9
        out += "\t"        #10
        out += context + "\t"        #11
        out += url        #12

        print(out)
