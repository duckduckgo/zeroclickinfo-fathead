#!/usr/bin/python
from bs4 import BeautifulSoup
import os
import sys
import string
import re

BASE_JAVADOC_URL = "https://docs.oracle.com/javase/8/docs/api/index.html?"
BASE_LOCAL_JAVADOC_DIR = "./docs/api/"

def collectDocFilesFrom(dir):
  docFiles = []
  for (path, dirs, files) in os.walk(dir):
    if 'class-use' not in path:
      for f in files:
        docFiles.append("%s/%s" % (path, f))
  return docFiles

def getClass(directory, fname):
  filename = "%s/%s" % (directory, fname)
  return getDocs(filename)

"""
Retrieves all methods of a specified class and saves them to methods.txt for coverage and appends formatted data to output.txt.
parameters: name of class
"""
def getClassMethods(filename, classname):
    content = BeautifulSoup(getcontent(classname), "html.parser")
    # Note: this will not find methods inherited from other classes/interfaces
    for method in content.find_all("table", {"summary" : re.compile("method")}):
        methodclass = getDocs(classname)[0]
        methodbaseurl = getDocs(classname)[2]
        method_output(filename, extractMethodData(method, methodclass, methodbaseurl, True))
        method_output('output.txt', extractMethodData(method, methodclass, methodbaseurl, False))
"""
Extracts data of a method, formatting it for either coverage or output.
parameters: method html table entry, class, url of the class, boolean coverage
returns: all method data and the class it belongs to
"""
def extractMethodData(method, methodclass, baseurl, coverage):
    method_names = []
    method_class = remove_keywords(methodclass)
    if method_class is not "":
        if coverage is True:
            for td in method.find_all("td", {"class" : "colLast"}):
                method_names.append(method_class + " " + extractMethodName(td.find("code").text.replace(" ", "")))
                # method name without parameters:
                method_names.append(method_class + " " + td.find("a").text.replace(" ", ""))
        elif coverage is False:
            for td in method.find_all("td", {"class" : "colLast"}):
                method_names.append(method_class + " " + format(td, baseurl, True))
                method_names.append(method_class + " " + format(td, baseurl, False))
    return method_names

"""
Extracts a method's name and parameters
parameters: data entry of a method
returns: formatted method and parameters for output.txt and methods.txt
"""
def extractMethodName(tabledata):
    methodname = ""
    if "()" in tabledata:
        methodname = tabledata.replace("\n", "").replace("()", "")
    else:
        methodname = tabledata.replace("\n", "").replace("&nbsp;", "").replace(")", "").replace("(", " ")
    return methodname

"""
Formats output for method data to be appended to output.txt
parameters: data entry of a method, base class url needed to construct a full url for the method, boolean determines whether parameters are to be included or not
returns: a formatted string for output.txt
"""
def format(tabledata, baseurl, parameters):
    method_name = ""
    if parameters is True:
        method_name = extractMethodName(tabledata.find("code").text)
    elif parameters is False:
        method_name = str(tabledata.find("span").text)
    method_description = ""
    if tabledata.find("div") is not None:
        method_description = tabledata.find("div").get_text().replace("\n", "")
    url_matcher = re.search(r'#(.*)', tabledata.find("a").get("href"))
    method_url = baseurl.replace("index.html?", "") + url_matcher.group(0)
    formatted_string = method_name + "\tA\t\t\t\t\t\t\t\t\t" + method_description + "\t" + method_url
    return formatted_string
    
"""
Appends a formatted line of data of a method to a specified file
parameters: filename (output.txt), list of method data
"""
def method_output(file, data):
    f = open(file, 'a')
    for d in data:
        method_data = d + "\n"
        for line in method_data:
             f.write(line)
    f.close()

def getDocs(filename):
  if filename.endswith('.html') and 'package-' not in filename and 'doc-files' not in filename:
    content = BeautifulSoup(getcontent(filename), 'html.parser')
    classname = content.find_all('h2')[0].string
    block = content.find_all('div', 'block', limit=1)
    description = ""
    if len(block) > 0:
      description = block[0].get_text()
      description = cutlength(description)
    url = BASE_JAVADOC_URL + filename.replace(BASE_LOCAL_JAVADOC_DIR, "")
    return classname, description, url

def cutlength(description):
#  if len(description) > 100:
  description = description[0:description.rfind('.', 0, 300) + 1]
  return description.replace("\n", "")

def remove_keywords(line):
  if isinstance(line, str): # replaced basestring with str. basestring is deprecated in python 3.x
    line = re.sub(r'<\w,?\w?>', '', line)
    return line.replace('Class ', '').replace('Enum ', '').replace('Interface ', '').replace('Annotation Type ', '').replace(' ', '')
  else:
    return ''

def getcontent(filename):
  f = open(filename, 'rb')
  # added 'b' to option. forces bytes rather than string.
  lines = f.read()
  f.close()
  return lines

def concat_list(data_list = ['', '', '']):
  if data_list != None:
    return concat(data_list[0], data_list[1], data_list[2])
  else:
    return ""

def concat(clazz, description, url):
  title = remove_keywords(clazz) or 'No class found'
  typez = 'A'
  redirect = ''
  four = ''
  categories = ''
  six = ''
  related_topics = '' # [[Perl Data Language|PDL]], can be multiples?
  eight = ''
  external_links = '' # [$url title text]\\n, can be multiples
  ten = ''
  image = ''
  abstract = description.replace("\n", "\\n").replace("\t", "\\t") or "No abstract found"
  abstract = '<section class="prog__container">' + abstract + '</section>'
  url = url or "No URL found"

  data = [title, typez, redirect, four, categories, six, related_topics, eight, external_links, ten, image, abstract, url]
  line = "\t".join(data) + "\n"
  return line

def output(filename, data_list):
  line = concat_list(data_list)
  if not line.startswith("No class found") and line != "" and not ("No abstract found" in line):
    f = open(filename, 'ab')
    f.write(line.encode('utf'))
    f.close()
