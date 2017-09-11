#!/usr/bin/python
from bs4 import BeautifulSoup
import urllib.request

BASE_JAVADOC_URL = "http://repo.spring.io/libs-release/org/springframework/spring/"
DOWNLOAD_DIR = "./download/"
BASE_JAVADOC_FILE = "./download/overview-summary.html"


def read_root_file():
    root_file = open(BASE_JAVADOC_FILE, 'r')
    lines = root_file.read()
    root_file.close()
    return lines

if __name__ == '__main__':
    content = BeautifulSoup(read_root_file(), 'html.parser')

    spring_release = content.find_all("h1", "title")[0].get_text()
    tokens = spring_release.split()
    spring_release = tokens[2]

    docsUrl = BASE_JAVADOC_URL + spring_release + "/spring-framework-" + spring_release + "-docs.zip"
    docsLocalFile = DOWNLOAD_DIR + "spring-framework-" + spring_release + "-docs.zip"

    print ("Fetching... " + docsUrl)
    result = urllib.request.urlretrieve(docsUrl, docsLocalFile)
    print ("Fetched... " + docsLocalFile)
