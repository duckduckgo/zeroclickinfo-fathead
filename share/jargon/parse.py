#!/usr/bin/python3

import bs4
import re

see_also_re = re.compile("\s+(?:\()?[Ss]ee also.*(  |$|\))")
nonletter_re = re.compile("[^A-Za-z]")

def trunc(s):    #these lengths are completely arbitrary
    s = s[:200]                        #chop at 200 characters
    x = s.rsplit(".  ", 1)
    if len(x[0]) > 160:                #if the last sentence break is more than 160 chars from the start
        return x[0] + "."
    elif len(s) == 200:                #if we ran right up to the end
        return s.rsplit(" ", 1)[0] + "..."
    else:
        return s

soup = bs4.BeautifulSoup(open("download/jargon.xml"))

for entry in soup.glossary.find_all("glossentry"):
    term = entry.glossterm.get_text()
    if entry.abbrev:
        grammar = entry.abbrev.find("emphasis", role="grammar")

        if grammar:
            grammar = grammar.get_text()
        if grammar == None:
            grammar = ""

    text = " ".join([x.get_text() for x in entry.find_all("glossdef")]) #concatenate all definitions
    text = " ".join([x.strip() for x in text.split("\n")])              #elide line breaks
    text = see_also_re.sub("", text)                                    #remove "see also", since we're including that in a separate field
    if grammar:
        text = "%s %s" % (grammar, text)                                #prepend grammar
    text = trunc(text)                                                  #truncate

    see_also = "\\\\n".join(["[[%s]]" % x.get_text().replace("\n", "").replace("   ", " ") for x in entry.find_all("glossterm")][1:])    #the first one is the current term, so we skip that

    letter = term[0].upper()
    m = nonletter_re.match(letter)
    if m:
        letter = "0"
    url = "http://catb.org/jargon/html/%s/%s.html" % (letter, entry.get("id"))

    out = "\t".join([
        term,       #0
        "A",        #1
        "",         #2
        "",         #3
        "",         #4
        "",         #5
        see_also,   #6
        "",         #7
        "",         #8
        "",         #9
        "",         #10
        text,       #11
        url])       #12

    print(out)
