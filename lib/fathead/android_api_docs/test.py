#!/usr/bin env python
# -*- coding: utf-8 -*-
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
    '''Builds fathead article entry.

    '''

    '''
    title = tr.td.a.contents[0]
    url = (tr.find('a')['href'])
    #Some packages don't have a description, but simply a page listing classes and interfaces (possibly enums)
    try:
        abstract = tr.find('p').contents[0]
    except:
        abstract = "More information about interfaces and classes at " + url
    title = '<span class="prog__sub">%s</span>' % title
    #Replace newline characters with a space
    abstract = re.sub('\n', ' ', abstract)
    abstract = abstract.strip()

    abstract = '<p>%s</p>' % abstract
    abstract = '<section class="prog__container">%s</section>' % abstract
    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
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
    '''

    title = str(soup.find('h1').contents[0])

    filePath = filePath[11:]
    url = baseurl + filePath
    abstract = "Hey"
    firstCode = soup.find('code')
    secondCode = firstCode.find_next_sibling().find('code')
    firstP = soup.find('p').contents[0]

#Credit for crlf removal: http://stackoverflow.com/questions/16149649/remove-carriage-return-in-python
    print(''.join(firstCode.contents[0].splitlines()))

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

'''
with open('output.txt', 'w') as fp:
    for html_file in glob('download/*.html'):
        print('Processing %s' % html_file)
        soup = BeautifulSoup(open(html_file), 'html.parser')
        print('Page url %s' % page_url)
        trs = soup.findAll("tr")
        for tr in trs:
            data = build_article(tr)
            print(data)
            data = '\t'.join(data)
            fp.write('{}\n'.format(data))
'''

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
