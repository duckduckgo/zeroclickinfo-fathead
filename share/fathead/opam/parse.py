#!/usr/bin/python
from __future__ import print_function
import os

from bs4 import BeautifulSoup

__module__ = "OPAM Packages"
__author__ = "PJ Hampton"

# check if file exists
# if it doesn, delete it.

if os.path.exists("output.txt"):
    os.remove("output.txt")

# open downloaded file
# read contents and close.

f = open('download/packages.html')
contents = f.read()
f.close()

soup = BeautifulSoup(contents, 'html.parser')

# scrape data from html document

data = []
table = soup.find('table', attrs={'id':'packages'})
table_body = table.find('tbody')

rows = table_body.find_all('tr')
for row in rows:
    cols = row.find_all('td')
    cols = [ele.text.strip() for ele in cols]
    data.append(cols)

# write data to file

with open("output.txt", "w") as f:
    for item in data:
        print ("%s\t%s\t%s" % (
            item[0].encode('utf-8').decode('ascii', 'ignore'),
            item[1].encode('utf-8').decode('ascii', 'ignore'),
            item[2].encode('utf-8').decode('ascii', 'ignore')
        ), file=f)