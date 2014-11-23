#!/usr/bin/python
import os
from bs4 import BeautifulSoup
import sys
import string

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

def getDocs(filename):
  if filename.endswith('.html') and 'package-' not in filename and 'doc-files' not in filename:
    content = BeautifulSoup(getcontent(filename))
    classname = content.find_all('h2')[0].string
    block = content.find_all('div', 'block', limit=1)
    if len(block) > 0:
      description = block[0].get_text()
      description = description[0:description.find('.', 100) + 1].replace("\n", "")
    else:
      description = 'no description found'
    url = "https://docs.oracle.com/javase/8/docs/api/index.html?" + filename.replace("./docs/api/", "")
    return classname, description, url
  else:
    return ["", "", ""]

def getcontent(filename):
  f = open(filename, 'r')
  lines = f.read()
  f.close()
  return lines

def concat_list(data_list = ['', '', '']):
  return concat(data_list[0], data_list[1], data_list[2])

def concat(clazz, description, url):
  title = clazz or 'No class found'
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
  url = url or "No URL found"
  
  data = [title, typez, redirect, four, categories, six, related_topics, eight, external_links, ten, image, abstract, url]
  line = "\t".join(data) + "\n"
  return line

def output(filename, data_list):
  f = open(filename, 'a')
  line = concat_list(data_list)

  f.write(line.encode('utf'))
  f.close()


# delete previous output
os.remove('output.txt')
# iterate package "java"
for f in collectDocFilesFrom('./docs/api/java'):
  output("output.txt", getDocs(f))

# iterate package "javax"
for f in collectDocFilesFrom('./docs/api/javax'):
  output("output.txt", getDocs(f))

