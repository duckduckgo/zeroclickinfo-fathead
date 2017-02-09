#!/usr/bin/env python3

baseUrl = 'https://pip.pypa.io/en/stable/reference/'
import urllib.request
from bs4 import BeautifulSoup

response = urllib.request.urlopen(baseUrl)
page_source = response.read()

download = "download/"



soup = BeautifulSoup(page_source,"lxml")

mains = soup.find('li', {"class":"toctree-l1 current"})


urls = []
for a in mains.find('ul').find_all('a', href=True):
    #print "Found the URL:", a['href']
    urls.append(a['href'])

out  = {}
for url in urls:

    response = urllib.request.urlopen(baseUrl + url)
    page_source = response.read()
    filename = download + url[:-1] + '.html'
    temp = open(filename,"w")

    temp.write(page_source.decode('utf-8'))
    temp.close()
