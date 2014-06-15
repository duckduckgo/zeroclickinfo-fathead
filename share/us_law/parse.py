#!/usr/bin/env python3

from bs4 import BeautifulSoup
import os

def find_parents(n, parents):
	parents.insert(0, (n.parent.name, n.parent.num, n.parent.heading))

	if n.parent.name == "title":	#top level
		return parents
	else:
		return find_parents(n.parent, parents)

for title in os.listdir("download/"):
	soup = BeautifulSoup(open("download/%s" % title))
	if soup.appendix:
		continue

	for section in soup.find_all("section"):
		if section.get("id") == None:
			continue	#not a leaf node

		chain = find_parents(section, []) + [(section.name, section.num, section.heading)]

		num_title = chain[0][1].get("value")
		num_sec = chain[-1][1].get("value")

		out = "\t".join([x[1].get_text() + " " + x[2].get_text() for x in chain])
		url = "http://www.law.cornell.edu/uscode/text/%s/%s" % (num_title, num_sec)
		print("%s\t%s\t%s\t%s" % (num_title, num_sec, out, url))
