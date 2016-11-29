#!/usr/bin/python

# Fetches individual docs found in 'allclasses-noframe.html' and places them into the 'docs' folder.

from bs4 import BeautifulSoup
import urllib

BASE_JAVADOC_FILE = "./download/allclasses-noframe.html"
BASE_JAVADOC_URL = "http://docs.spring.io/spring/docs/current/javadoc-api/"
BASE_LOCAL_JAVADOC_DIR = "./docs/api/"


def readRootFile():
    rootFile = open(BASE_JAVADOC_FILE, 'r')
    lines = rootFile.read()
    rootFile.close()
    return lines


content = BeautifulSoup(readRootFile(), 'html.parser')
for classUrl in content.find_all('a'):
    localClassFile = BASE_LOCAL_JAVADOC_DIR + classUrl.get_text() + ".html"
    classUrl = BASE_JAVADOC_URL + classUrl.get('href')
    print ("Fetching... " + classUrl)
    result = urllib.urlretrieve(classUrl, localClassFile)
    print ("Fetched... " + localClassFile)
