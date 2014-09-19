#!/usr/bin/env python3

abbrs = {"genesis": ["Gen"], "exodus": ["Exd"], "leviticus": ["Lev"], "numbers": ["Num"], "deuteronomy": ["Deu", "Deut"], "joshua": ["Jos", "Josh"], "judges": ["Jdg", "Judg"], "ruth": ["Rth", "Ruth"], "1 samuel": ["1Sa", "1Sam", "1 Sam"], "2 samuel": ["2Sa", "2Sam", "2 Sam"], "1 kings": ["1Ki", "1Kings", "1 Kings"], "2 kings": ["2Ki", "2Kings", "2 Kings"], "1 chronicles": ["1Ch", "1Chr", "1 Chr"], "2 chronicles": ["2Ch", "2Chr", "2 Chr"], "ezra": ["Ezr", "Ezra"], "nehemiah": ["Neh"], "esther": ["Est", "Esth"], "psalms": ["Psa", "Ps"], "proverbs": ["Pro", "Prov"], "ecclesiastes": ["Ecc", "Eccl"], "song of solomon": ["Sgs", "Song"], "sirach": ["Sir"], "isaiah": ["Isa"], "jeremiah": ["Jer"], "lamentations": ["Lam"], "baruch": ["Bar"], "ezekiel": ["Eze", "Ezek"], "daniel": ["Dan"], "hosea": ["Hsa", "Hos"], "joel": ["Joe"], "amos": ["Amo"], "obadiah": ["Oba"], "jonah": ["Jon"], "micah": ["Mic"], "nahum": ["Nah"], "habakkuk": ["Hab"], "zephaniah": ["Zep", "Zeph"], "haggai": ["Hag"], "zechariah": ["Zec", "Zech"], "malachi": ["Mal"], "matthew": ["Mat", "Mt"], "mark": ["Mar", "Mk"], "luke": ["Luk", "Lk"], "john": ["Jhn", "Jn"], "acts": ["Act"], "romans": ["Rom"], "1 corinthians": ["1Cr", "1Cor", "1 Cor"], "2 corinthians": ["2Cr", "2Cor", "2 Cor"], "galatians": ["Gal"], "ephesians": ["Eph"], "philippians": ["Phl", "Phil"], "colossians": ["Col"], "1 thessalonians": ["1Th", "1Thess", "1 Thess"], "2 thessalonians": ["2Th", "2Thess", "2 Thess"], "1 timothy": ["1Ti", "1Tim", "1 Tim"], "2 timothy": ["2Ti", "2Tim", "2 Tim"], "titus": ["Tts"], "philemon": ["Phm"], "hebrews": ["Hbr", "Heb"], "james": ["Jam", "Jas"], "1 peter": ["1Pe", "1Pt", "1 Pt"], "2 peter": ["2Pe", "2Pt", "2 Pt"], "1 john": ["1Jo", "1Jn", "1 Jn"], "2 john": ["2Jo", "2Jn", "2 Jn"], "3 john": ["3Jo", "3Jn", "3 Jn"], "jude": ["Jud", "Jude"], "revelation": ["Rev"]}

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
        shortnames = abbrs[book_name.lower()]
        for ab in shortnames:
            print_redir("%s %s:%s" % (ab, chap_num, verse_num), key)
            print_redir("%s %s %s" % (ab, chap_num, verse_num), key)
    except KeyError:
        shortnames = None

    #we also want entries for entire chapters, but the input file only contains verses
    #so every time we get to a new chapter, insert a record for the chapter
    if chap_num != prev_chap or book_name != prev_book:
        key = "%s %s" % (book_name, chap_num)
        url = "http://blb.org/search/preSearch.cfm?Criteria=%s+%s" % (book_name, chap_num)
        text += "..." #text already contains the text of the first verse of the chapter, which is probably good enough
        print_line(key, text, url)
        if shortnames:
            for ab in shortnames:
                print_redir("%s %s" % (ab, chap_num), key)

    prev_chap = chap_num
    prev_book = book_name
