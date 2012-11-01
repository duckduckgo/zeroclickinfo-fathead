#!/usr/bin/env python
import csv

reader = csv.reader(open("download/kjv.txt", "rb"), delimiter="\t")
output = open("output.txt", "wb")

abv = {
"genesis": "Gen", "exodus": "Exd", "leviticus": "Lev", "numbers": "Num", "deuteronomy": "Deu", "joshua": "Jos", "judges": "Jdg", "ruth": "Rth", "1 samuel": "1Sa", "2 samuel": "2Sa", "1 kings": "1Ki", "2 kings": "2Ki", "1 chronicles": "1Ch", "2 chronicles": "2Ch", "ezra": "Ezr", "nehemiah": "Neh", "esther": "Est", "job": "Job", "psalms": "Psa", "proverbs": "Pro", "ecclesiastes": "Ecc", "song of solomon": "Sgs", "isaiah": "Isa", "jeremiah": "Jer", "lamentations": "Lam", "ezekiel": "Eze", "daniel": "Dan", "hosea": "Hsa", "joel": "Joe", "amos": "Amo", "obadiah": "Oba", "jonah": "Jon", "micah": "Mic", "nahum": "Nah", "habakkuk": "Hab", "zephaniah": "Zep", "haggai": "Hag", "zechariah": "Zec", "malachi": "Mal", "matthew": "Mat", "mark": "Mar", "luke": "Luk", "john": "Jhn", "acts": "Act", "romans": "Rom", "1 corinthians": "1Cr", "2 corinthians": "2Cr", "galatians": "Gal", "ephesians": "Eph", "philippians": "Phl", "colossians": "Col", "1 thessalonians": "1Th", "2 thessalonians": "2Th", "1 timothy": "1Ti", "2 timothy": "2Ti", "titus": "Tts", "philemon": "Phm", "hebrews": "Hbr", "james": "Jam", "1 peter": "1Pe", "2 peter": "2Pe", "1 john": "1Jo", "2 john": "2Jo", "3 john": "3Jo", "jude": "Jud", "revelation": "Rev"

}

def newStr(name, theType, redirect, category, verse, url):
    """ Update string formatting.
    """
    ddgStr = [      name,                   # name
                    theType,                # type
                    redirect,               # redirect
                    "",                     # otheruses
                    category,               # categories
                    "",                     # references
                    "",                     # see_also
                    "",                     # further_reading
                    "",                     # external_links
                    "",                     # disambiguation
                    "",                     # images
                    verse,                  # abstract
                    url+"\n"                # source_url
             ]
    return "%s" % ("\t".join(ddgStr))

for row in reader:
     book = row[0] # e.g. Genesis
     chapter = "{0}:{1}".format(row[1],row[2]) # e.g. 1:23
     name = "{0} {1}".format(book, chapter)
     verse = row[3]
     url = "http://blb.org/search/preSearch.cfm?Criteria={0}+{1}".format(book,chapter)

     chapterSpace = "{0} {1}".format(row[1],row[2]) # e.g. 1 23
     nameSpace = "{0} {1}".format(book, chapterSpace)

     temp = newStr(name, "A", "", "Bible Verses\\n", verse, url) + \
            newStr(nameSpace, "R", name, "", "", "")

     output.write(temp)

     if book != "Job":
          abvBook = abv[book.lower()] # e.g. Gen
          abvName = "{0} {1}".format(abvBook, chapter)
          abvUrl = "http://blb.org/search/preSearch.cfm?Criteria={0}".format(abvName)
          abvNameSpace = "{0} {1}".format(abvBook, chapterSpace)
          temp = newStr(abvName, "R", name, "", "", "") + \
                 newStr(abvNameSpace, "R", name, "", "", "")

          output.write(temp)
