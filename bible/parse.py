#!/usr/bin/env python
import csv

reader = csv.reader(open("download/kjv.txt", "rb"), delimiter='\t')
output = open('output.txt', 'wb')

abv = {
"genesis": "Gen", "exodus": "Exd", "leviticus": "Lev", "numbers": "Num", "deuteronomy": "Deu", "joshua": "Jos", "judges": "Jdg", "ruth": "Rth", "1 samuel": "1Sa", "2 samuel": "2Sa", "1 kings": "1Ki", "2 kings": "2Ki", "1 chronicles": "1Ch", "2 chronicles": "2Ch", "ezra": "Ezr", "nehemiah": "Neh", "esther": "Est", "job": "Job", "psalms": "Psa", "proverbs": "Pro", "ecclesiastes": "Ecc", "song of solomon": "Sgs", "isaiah": "Isa", "jeremiah": "Jer", "lamentations": "Lam", "ezekiel": "Eze", "daniel": "Dan", "hosea": "Hsa", "joel": "Joe", "amos": "Amo", "obadiah": "Oba", "jonah": "Jon", "micah": "Mic", "nahum": "Nah", "habakkuk": "Hab", "zephaniah": "Zep", "haggai": "Hag", "zechariah": "Zec", "malachi": "Mal", "matthew": "Mat", "mark": "Mar", "luke": "Luk", "john": "Jhn", "acts": "Act", "romans": "Rom", "1 corinthians": "1Cr", "2 corinthians": "2Cr", "galatians": "Gal", "ephesians": "Eph", "philippians": "Phl", "colossians": "Col", "1 thessalonians": "1Th", "2 thessalonians": "2Th", "1 timothy": "1Ti", "2 timothy": "2Ti", "titus": "Tts", "philemon": "Phm", "hebrews": "Hbr", "james": "Jam", "1 peter": "1Pe", "2 peter": "2Pe", "1 john": "1Jo", "2 john": "2Jo", "3 john": "3Jo", "jude": "Jud", "revelation": "Rev"

}


# name\tA\t\t\tBible Verses\\n\t\t\t\t\t\t\t\t\tverse\turl\n

ddgStr = "\tA\t\t\tBible Verses\\n\t\t\t\t\t\t\t\t\t"

for row in reader:
     book = row[0]
     chapter = "{0}:{1}".format(row[1],row[2])

     verse = row[3]
     url = "http://blb.org/search/preSearch.cfm?Criteria={0}+{1}".format(book,chapter)
     temp = "{0} {1}{2}{3}\t{4}\n".format(book,chapter,ddgStr,verse,url)
     output.write(temp)

     if book != "Job":
          abvName = abv[book.lower()]
          abvUrl = "http://blb.org/search/preSearch.cfm?Criteria={0}+{1}".format(abvName,chapter)
          redirect = "{0} {1}".format(book,chapter)
          ddgStrRedir = "\tR\t{0}\t\t\t\\n\t\t\t\t\t\t\t\t\t".format(redirect)
          abvTemp = "{0} {1}{2}\n".format(abvName,chapter,ddgStrRedir)
          output.write(abvTemp)
