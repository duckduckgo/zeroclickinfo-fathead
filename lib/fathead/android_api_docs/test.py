#!/usr/bin env python
# -*- coding: utf-8 -*-
'''
    Coulter Peterson
'''
from bs4 import BeautifulSoup
from glob import glob
import re
# Import the os module, for the os.walk function
import os

baseurl = 'https://developer.android.com/'

def build_summary_article(soup, filePath):

    title = str(soup.find('h1').contents[0])

    filePath = filePath[11:]
    url = baseurl + filePath
    abstract = str(soup.find('p').contents[0])

    return  [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            url,             # anchor to specific section
        ]

def build_article(soup, filePath):

    title = str(soup.find('h1').contents[0])

    filePath = filePath[11:]
    url = baseurl + filePath

    #First line of expample code listed
    firstCode = soup.find('code')
    #Second line of example code in doc
    secondCode = firstCode.find_next_sibling().find('code')

    #Credit for crlf removal: http://stackoverflow.com/questions/16149649/remove-carriage-return-in-python
    #Credit for the sporadic whitespace removal: http://stackoverflow.com/questions/2077897/substitute-multiple-whitespace-with-single-whitespace-in-python
    firstCode = ''.join(firstCode.contents[0].splitlines())
    firstCode = ' '.join(firstCode.split())

    #Credit for full inner html: http://stackoverflow.com/questions/8112922/beautifulsoup-innerhtml
    secondCode = secondCode.decode_contents(formatter="html")
    #Remove carriage returns and extra whitespace
    secondCode = ''.join(secondCode.splitlines())
    secondCode = ' '.join(secondCode.split())

    classDescription = soup.find("hr").find_next("p")
    classDescription = classDescription.prettify()

    #Google unforunately didn't close quite a few p tags, so we need to grab the description contents this way
        #Credit for partition: http://stackoverflow.com/questions/14801057/python-splitting-to-the-newline-character
    try:
        classDescription = classDescription.partition("<h2")[0]
    except:
        print("No h2")
    try:
        classDescription = classDescription.partition("<h3")[0]
    except:
        print("No 33")
    try:
        classDescription = classDescription.partition("<div")[0]
    except:
        print("No div")
    #Add missing closing p tags where needed
    try:
        classDescription = classDescription.partition("</p>")[0]
        classDescription += "</p>"
    except:
        print("No closing </p>")

    #Remove carriage returns and extra whitespace
    classDescription = ''.join(classDescription.splitlines())
    classDescription = ' '.join(classDescription.split())

    #Use DuckDuckHack's recommended code snippet wraps
    classDescription = classDescription.replace("<code>", "<pre><code>")
    classDescription = classDescription.replace("</code>", "</code></pre>")

    #Build the abstract from the description and example code usage
    abstract = ""
    abstract += str(classDescription) + "\\n"
    abstract += "<pre><code>%s</code></pre>" % (str(firstCode) + "\\n" + str(secondCode))
    abstract = '<section class="prog__container">%s</section>' % abstract

    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    print("=======================================================================")

    return  [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            url,             # anchor to specific section
        ]

with open('output.txt', 'w') as fp:
    # Set the directory you want to start from
    rootDir = './download/reference/android/accessibilityservice'
    for dirName, subdirList, fileList in os.walk(rootDir):
        #print('Found directory: %s' % dirName)

        #These are all .html files in a package, one of which is package-summary.html
        for fname in fileList:
            filePath = dirName + "/" + fname
            soup = BeautifulSoup(open(filePath), 'html.parser')
            #The package summary needs to be built differently
            if (fname == "package-summary.html"):
                print("\tThe summary file:" + fname)
                data = build_summary_article(soup, filePath)
                data = '\t'.join(data)
                fp.write('{}\n'.format(data))
            else:
                #Build regular article with code highlighting
                print('\tThe file:%s' % fname)
                data = build_article(soup, filePath)
                data = '\t'.join(data)
                fp.write('{}\n'.format(data))
