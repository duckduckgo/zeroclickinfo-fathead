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
#
# Updates:
#
# August 30, 2016
# - Now works with DocSets included in XCode 7.3.1

import sqlite3
import re
import sys

reload(sys)
sys.setdefaultencoding('utf8')

# These contains all of the API documentation
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
    redirect_format = "{alt_name}\tR\t{name}\t\t\t\t\t\t\t\t\t\t\n"

    f = open('output.txt', 'a')

    for r in result:
        if r['redirect']:
            for alt_name in r['alt_names']:
                r['alt_name'] = alt_name
                f.write(redirect_format.format(**r))
        f.write(abstract_format.format(**r))

    f.close()

def create_fathead(database, platform):
    # Connect to the documentation's sqlite database.
    conn = sqlite3.connect(database)
    c = conn.cursor()

    # Variables that we need for later.
    result = []

    seen_list = {}

    # This long SQL query just gets the details about each class and method.
    # ZLANGUAGE = 3 means Swift
    # Note: The SQL query is different between different docsets.
    for row in c.execute('''SELECT ZTOKENNAME, ZABSTRACT, ZTOKENMETAINFORMATION.ZANCHOR, ZDECLARATION, ZNODEURL.ZPATH, ZTOKENUSR, ZTOKEN.ZTOKENTYPE 
                            FROM ZTOKEN, ZTOKENMETAINFORMATION, ZNODEURL 
                            WHERE ZLANGUAGE=3 
                            AND ZTOKENTYPE IN (1, 4, 5, 7, 8, 12, 14, 17) 
                            AND ZTOKENMETAINFORMATION.ZTOKEN=ZTOKEN.Z_PK 
                            AND ZTOKENNAME IS NOT NULL 
                            AND ZABSTRACT IS NOT NULL 
                            AND ZTOKENMETAINFORMATION.ZANCHOR IS NOT NULL 
                            AND ZDECLARATION IS NOT NULL 
                            AND ZTOKENUSR IS NOT NULL 
                            AND ZNODEURL.ZPATH IS NOT NULL
                            AND ZNODEURL.ZNODE=ZTOKEN.ZPARENTNODE
                            ORDER BY ZTOKENNAME'''):
        name, abstract, anchor, snippet, path, usr, tokentype = row

        # This is the meta data that we're going to attach later.
        pack = {
            "name": name,
            "abstract": abstract or "",
            "path": urls[platform] + path + "#" + anchor,
            "original": abstract or "",
            "platform": platform,
            "snippet": snippet or "",
        }
        
        # Remove all the tags inside the pre tags.
        snippet = re.sub(r'<[^>]*>', '', snippet)
        pack['snippet'] = "<pre><code>" + snippet + "</code></pre>"

        # Process the abstract
        # Classes have irrelevant snippets so we're not adding that in
        if tokentype != 12:
            pack['abstract'] = pack['abstract'] + " " + pack['snippet']
        pack['abstract'] = pack['abstract'].replace("\n", "\\n")

        # Remove function parenthesis
        p = re.compile('\(.*?\)')
        pack['name'] = re.sub(p, '', pack['name'])

        # Have we seen this before?
        if not pack['name'] in seen_list:
            seen_list[pack['name']] = True
        else:
            continue

        # ----------------
        # Create redirects
        # ----------------
        # 
        # This is where the variable `usr` will come in handy. It has information on whether a certain 
        # method or variable belongs to a class which makes it useful for queries like "length nsstring",
        # "nsstring length", or "nsstring.length" 

        p = re.compile('c:objc\(..\)([a-zA-Z]+)\(..\)([a-zA-Z]+)')
        if p.match(usr):
            pack['redirect'] = p.findall(usr)
            cl = pack['redirect'][0][0]
            prop = pack['redirect'][0][1]
            pack['name'] = cl + "." + prop

            pack['alt_names'] = [cl + " " + prop, prop + " " + cl]
        else:
            pack['redirect'] = None

        # Log
        print pack['name']

        # This variable gets an array.
        # First element is the class, and the second one is the method.
        result.append(pack)

    conn.close()
    generate_output(result)

# Only create Fathead for iOS
create_fathead(ios, 'iOS')