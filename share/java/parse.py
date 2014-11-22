#!/usr/bin/python
import os
import re
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
    description = content.find_all('div', 'description', limit=1)[0].get_text()
    return classname, description
  else:
    return ""

def getcontent(filename):
  f = open(filename, 'r')
  lines = f.read()
  f.close()
  return lines

def output(filename, (clazz, description)):
  f = open(filename, 'a')
  line = clazz + "\t" + description.replace("\n", "\\n").replace("\t", "\\t")
  line += "\n"
  f.write(line.encode('utf'))
  f.close()

files = []
files.append(collectDocFilesFrom('./docs/api/java'))
files.append(collectDocFilesFrom('./docs/api/javax'))

output("output.txt", getDocs('./docs/api/java/lang/String.html'))

