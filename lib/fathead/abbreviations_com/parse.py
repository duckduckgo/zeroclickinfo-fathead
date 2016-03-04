#!/usr/bin/env python

def print_line(key, text, url):
    print("\t".join([
        key,   #1
        "A",   #2
        "",    #3
        "",    #4
        "",    #5
        "",    #6
        "",    #7
        "",    #8
        "",    #9
        "",    #10
        "",    #11
        text,  #12
        url])) #13

with open("download/abbr.txt") as f:
    lines = f.readlines()

for line in lines:
    fields = line.split("\t")

    key = fields[0]
    text = fields[1]
    url = "http://www.abbreviations.com/%s" % (key)

    print_line(key, text, url)      #print entry