#!/usr/bin/env python3

from bs4 import BeautifulSoup
import os

def find_parents(n, parents):
    parents.insert(0, (n.parent.name, n.parent.num, n.parent.heading))

    #if n.parent.name in ["title", "appendix"]:    #top level
    if n.parent.name == "title":    #top level
        return parents
    else:
        return find_parents(n.parent, parents)

for title in os.listdir("download/xml/"):
    soup = BeautifulSoup(open("download/xml/%s" % title))

    if soup.appendix:   #Appendices aren't part of the code
        continue


    """
    if soup.appendix and soup.appendix.num.value in ["11a", "28a"]:
        leaf_element = "courtRule"  #Appendices don't conform to the pattern
    else:
        leaf_element = "section"
    """

    for section in soup.find_all("section"):
        if section.get("id") == None:
            continue    #not a leaf node

        #build a chain from the section to the top level. We don't know how many intermediate links there will be.
        chain = find_parents(section, []) + [(section.name, section.num, section.heading)]

        num_title = chain[0][1].get("value")    #One end of the chain is the title...
        num_sec = chain[-1][1].get("value")     #...and the other end is the section

        context = "<br>".join([x[1].get_text() + " " + x[2].get_text() for x in chain])    #Combine the headings of every level of the chain
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
