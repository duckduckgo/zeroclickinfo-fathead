__author__ = 'kchahal'
#To change this template use Tools | Templates.

baseUrl = 'https://pip.pypa.io/en/stable/reference/'
import urllib2
from bs4 import BeautifulSoup
response = urllib2.urlopen(baseUrl)
page_source = response.read()
main = "main.html"
download = "download/"
f = open(main, "w");


soup = BeautifulSoup(page_source)

mains = soup.find('li', {"class":"toctree-l1 current"})


urls = []
for a in mains.find('ul').find_all('a', href=True):
    #print "Found the URL:", a['href']
    urls.append(a['href'])

out  = {}
for url in urls:

    response = urllib2.urlopen(baseUrl + url)
    page_source = response.read()
    filename = download + url[:-1] + '.html'
    temp = open(filename,"w")

    temp.write(page_source)
    temp.close()
