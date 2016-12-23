#!/usr/bin/python
from bs4 import BeautifulSoup
import urllib

BASE_JAVADOC_URL = "http://repo.spring.io/libs-release/org/springframework/spring/"
DOWNLOAD_DIR = "./download/"
BASE_JAVADOC_FILE = "./download/overview-summary.html"


def readRootFile():
    rootFile = open(BASE_JAVADOC_FILE, 'r')
    lines = rootFile.read()
    rootFile.close()
    return lines


content = BeautifulSoup(readRootFile(), 'html.parser')

spring_release = content.find_all("h1", "title")[0].get_text()
tokens = spring_release.split()
spring_release = tokens[2]

docsUrl = BASE_JAVADOC_URL + spring_release + "/spring-framework-" + spring_release + "-docs.zip"
docsLocalFile = DOWNLOAD_DIR + "spring-framework-" + spring_release + "-docs.zip"

print ("Fetching... " + docsUrl)
result = urllib.urlretrieve(docsUrl, docsLocalFile)
print ("Fetched... " + docsLocalFile)
