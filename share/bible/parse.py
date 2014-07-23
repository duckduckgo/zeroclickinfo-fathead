#!/usr/bin/env python3

abbrs = {"genesis": "Gen", "exodus": "Exd", "leviticus": "Lev", "numbers": "Num", "deuteronomy": "Deu", "joshua": "Jos", "judges": "Jdg", "ruth": "Rth", "1 samuel": "1Sa", "2 samuel": "2Sa", "1 kings": "1Ki", "2 kings": "2Ki", "1 chronicles": "1Ch", "2 chronicles": "2Ch", "ezra": "Ezr", "nehemiah": "Neh", "esther": "Est", "psalms": "Psa", "proverbs": "Pro", "ecclesiastes": "Ecc", "song of solomon": "Sgs", "isaiah": "Isa", "jeremiah": "Jer", "lamentations": "Lam", "ezekiel": "Eze", "daniel": "Dan", "hosea": "Hsa", "joel": "Joe", "amos": "Amo", "obadiah": "Oba", "jonah": "Jon", "micah": "Mic", "nahum": "Nah", "habakkuk": "Hab", "zephaniah": "Zep", "haggai": "Hag", "zechariah": "Zec", "malachi": "Mal", "matthew": "Mat", "mark": "Mar", "luke": "Luk", "john": "Jhn", "acts": "Act", "romans": "Rom", "1 corinthians": "1Cr", "2 corinthians": "2Cr", "galatians": "Gal", "ephesians": "Eph", "philippians": "Phl", "colossians": "Col", "1 thessalonians": "1Th", "2 thessalonians": "2Th", "1 timothy": "1Ti", "2 timothy": "2Ti", "titus": "Tts", "philemon": "Phm", "hebrews": "Hbr", "james": "Jam", "1 peter": "1Pe", "2 peter": "2Pe", "1 john": "1Jo", "2 john": "2Jo", "3 john": "3Jo", "jude": "Jud", "revelation": "Rev"}

def print_line(key, text, url):
    print("\t".join([
        key,   #1
        "A",   #2
        "",    #3
        "",    #4
        "Bible Verses\\n",  #5
        "",    #6
        "",    #7
        "",    #8
        "",    #9
        "",    #10
        "",    #11
        text,  #12
        url])) #13

def print_redir(key, dest):
    print("\t".join([
        key,   #1
        "R",   #2
        dest,  #3
        "",    #4
        "",    #5
        "",    #6
        "",    #7
        "",    #8
        "",    #9
        "",    #10
        "",    #11
        "",    #12
        ""]))  #13


with open("download/kjv.txt") as f:
    lines = f.readlines()

prev_book = ""  #to detect when we get to the beginning of a new chapter
prev_chap = -1

for line in lines:
    fields = line.split("\t")

    book_name = fields[0]
    chap_num = fields[1]
    verse_num = fields[2]
    text = fields[3][0:-1] #omit terminal \n
    url = "http://blb.org/search/preSearch.cfm?Criteria=%s+%s:%s" % (book_name, chap_num, verse_num)


    key = "%s %s:%s" % (book_name, chap_num, verse_num)
    print_line(key, text, url)      #print verse
    print_redir("%s %s %s" % (book_name, chap_num, verse_num), key)    #with space instead of colon

    try:    #if there is an abbreviated form of the book name
        short = abbrs[book_name.lower()]
        print_redir("%s %s:%s" % (short, chap_num, verse_num), key)
        print_redir("%s %s %s" % (short, chap_num, verse_num), key)
    except KeyError:
        short = None

    #we also want entries for entire chapters, but the input file only contains verses
    #so every time we get to a new chapter, insert a record for the chapter
    if chap_num != prev_chap or book_name != prev_book:
        key = "%s %s" % (book_name, chap_num)
        url = "http://blb.org/search/preSearch.cfm?Criteria=%s+%s" % (book_name, chap_num)
        text += "..." #text already contains the text of the first verse of the chapter, which is probably good enough
        print_line(key, text, url)
        if short:
            print_redir("%s %s" % (short, chap_num), key)

    prev_chap = chap_num
    prev_book = book_name
