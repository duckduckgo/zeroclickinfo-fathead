# Introduction:
# -------------
# This is used to open an SQLite database that's installed on your computer to create a Fathead output
# that's specified in http://docs.duckduckhack.com/resources/fathead-overview.html
#
# Page Structure:
# ---------------
# There is no page structure here because we're essentially scraping a database. The only requirement is that you have XCode.
#
# Pipeline:
# ---------
# Read DB -> Scrape / Process -> Output
#
# How do I test this?
# ------------------
# You'll need to have XCode installed on your computer. It comes bundled with the documentation that we need.
#
# TODO:
# - Get disambiguations working
# - Get redirects working
# - Methods / properties should be matched with the class, i.e., "NSString length"

import sqlite3
import re
import sys

reload(sys)
sys.setdefaultencoding('utf8')

# These contains all of the API documentation for iOS 7.1. It was installed by Xcode.
# It only has classes and methods--it doesn't have actual tutorials.
ios = "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.iOS.docset/Contents/Resources/docSet.dsidx"
osx = "/Applications/Xcode.app/Contents/Developer/Documentation/DocSets/com.apple.adc.documentation.OSX.docset/Contents/Resources/docSet.dsidx"

# This is the link to the docs.
urls = {
    'iOS': "https://developer.apple.com/library/ios/",
    'Mac': "https://developer.apple.com/library/mac/",
}

# Format the output as specified in https://duck.co/duckduckhack/fathead_overview
def generate_output(result):
    abstract_format = "{name}\tA\t\t\t\t\t\t\t\t\t\t{abstract}\t{path}\n"

    f = open('output.txt', 'a')

    for r in result:
        f.write(abstract_format.format(**r))

    f.close()

def create_fathead(database, platform):
    # Connect to the documentation's sqlite database.
    conn = sqlite3.connect(database)
    c = conn.cursor()

    # Variables that we need for later.
    result = []

    # This long SQL query just gets the details about each class and method.
    for row in c.execute('''select ZTOKENNAME,ZABSTRACT,ZPATH,ZDECLARATION,ZTOKENMETAINFORMATION.ZANCHOR
                            from ZTOKEN,ZTOKENMETAINFORMATION,ZNODEURL
                            where ZTOKEN.ZLANGUAGE=2 and ZTOKENMETAINFORMATION.ZTOKEN=ZTOKEN.Z_PK and ZNODEURL.ZNODE=ZTOKEN.ZPARENTNODE'''):
        name, abstract, path, snippet, anchor = row

        # Skip items that are missing some data.
        if not name or not anchor:
            continue

        p = re.compile(r"^[a-z]+$", re.IGNORECASE);
        if len(p.findall(name)) == 0:
            continue

        if snippet:
            snippet = re.sub(r'<[^>]*>', '', snippet.encode('utf-8', 'ignore'))
            snippet = "<pre>" + snippet + "</pre>"

        # This is the meta data that we're going to attach later.
        pack = {
            "name": name,
            "abstract": abstract or "",
            "path": urls[platform] + path + "#" + anchor,
            "original": abstract or "",
            "platform": platform,
            "snippet": snippet or "",
        }

        # Make sure we encode everything in UTF-8
        for k,v in pack.items():
            pack[k] = v.encode('utf-8')

        if snippet:
            pack['abstract'] = pack['snippet'] + " " + pack['abstract']

        pack['abstract'] = pack['abstract'].replace("\n", "\\n")

        # This variable gets an array.
        # First element is the class, and the second one is the method.
        result.append(pack)

    conn.close()
    generate_output(result)

create_fathead(ios, 'iOS')
create_fathead(osx, 'Mac')
