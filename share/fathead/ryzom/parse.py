#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import json
import re
import codecs
from HTMLParser import HTMLParser

class PageReader(HTMLParser):
    def __init__(self):
        self.reset()
        self.fed = []
        self.do_text = True
        self.image = ""
        self.level = 0
        self.imageflag = False
    def handle_starttag(self, tag, attrs):
        if(tag == "p" or (self.do_text and (tag == "a" or tag=="span"))):
            self.do_text = True
        else:
            self.level = self.level + 1
            self.do_text = False
        if(tag == "a"):
            for item in attrs:
                if(item[0] == "class"):
                    if(item[1] == "image"):
                        self.imageflag = True
        if(tag == "img" and self.imageflag):
            for item in attrs:
                if(item[0] == "src"):
                    self.image = item[1]
                    self.imageflag = False
                    break
#        self.imageflag = False

    def handle_endtag(self, tag):
        self.level = self.level - 1
        if(self.level == 0):
            self.do_text = True
    def handle_data(self, d):
        if(self.do_text):
            self.fed.append(d)
    def get_data(self):
        return unicode('').join(self.fed)

def page(articlename, image, abstract, url, categories=[], articletype="A", alias="", external_links=""):
    s = (articlename + "\t" + 
            articletype + "\t" + 
            alias + "\t\t" +
            "\\n".join(categories) + "\t\t" +
            "" + "\t" +
            "" + "\t" +
            image + "\t" +
            abstract + "\t" +
            url
            )
    return s + "\n"

basepath = unicode("http://en.wiki.ryzom.com/wiki/")

if __name__ == "__main__":
    # Read categories
    with codecs.open("output.txt", "wt", "utf-8") as output:
        with open("downloads/categories.txt", "r") as categories_file:
            for line in categories_file.xreadlines():
                category = line.strip(" \n\t")
                output.write(page(category, "", "", basepath+"Category:"+category))
                for f in os.listdir("downloads/"+category):
                    fn = "downloads/"+category+"/"+f
                    if not os.path.isfile(fn):
                        continue
                    with open(fn, "r") as article:
                        a = json.loads(article.read())
                        a = a['parse']
                        title = a['displaytitle']
                        data = PageReader()
                        data.feed(a['text']['*'])
                        try:
                            text = re.match(r'(?:[^.:;]+[.:;]){2}', data.get_data()).group()
                        except:
                            text = ""
                        output.write(page(title, data.image, text.strip(" \t\n"), basepath + title, [category]))

