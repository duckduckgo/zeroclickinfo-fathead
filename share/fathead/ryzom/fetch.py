#!/usr/bin/env python

import os
import urllib2
import json

# Defines the categories to download and iterate over
categories = ["Herbivores", "Carnivores"]

basepath = "http://en.wiki.ryzom.com/w/api.php?format=json&"

def makepath(path):
    if not os.path.exists(path):
        os.makedirs(path)

def request(req):
    res = urllib2.urlopen(basepath + req)
    return res.read()

def fetch_category(category):
    makepath("downloads/" + category)
    res = request("action=query&list=categorymembers&"
            "cmlimit=100&cmtitle=Category:" +
            category)
    res = json.loads(res)
    for member in res['query']['categorymembers']:
        with open("downloads/"+category+"/"+str(member['pageid']), "wt") as member_file:
            member_file.write(request("action=parse&format=json&pageid=" + str(member['pageid'])))

if __name__ == "__main__":
    makepath("downloads")
    with open("downloads/categories.txt", "wt") as categories_list:
        for category in categories:
            fetch_category(category)
            categories_list.write(category+"\n")

