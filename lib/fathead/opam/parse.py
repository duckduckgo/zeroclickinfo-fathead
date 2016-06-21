#!/usr/bin/python

import os
from urlparse import urljoin

from bs4 import BeautifulSoup

__author__ = "http://github.com/pjhampton"
__target__ = "https://opam.ocaml.org/"
__module__ = "OPAM"

class Environment(object):
  """
  The Environment class is responsible for setting up the directory
  and retriving the appropriate files from the web. It is made up of
  a constructor and a 'setup/0' and 'get_contents/0' member functions.
  """
  def __init__(self):
    print("Environment instanciated")
    self.setup()

  def setup(self):
    """
    The setup member function checks to see if the 'output.txt' file
    exists. If it does, delete it and move on, otherwise move on. This
    is called in the constructor of the Environment object and shouldn't
    be called again.
    """
    if os.path.exists("output.txt"):
      os.remove("output.txt")

  def get_contents(self):
    """
    The 'get_contents/0' member function loads the file into memory and
    reads the contents. The file buffer is then closed and then a
    BeautifulSoup object is instanciated, with the contents. More on this
    can be read at the BeautifulSoup documentation website.
    """
    f = open('download/packages.html')
    contents = f.read()
    f.close()
    soup = BeautifulSoup(contents, 'html.parser')

    return soup

class Document(object):
  """
  The Document class is responsible for setting up the end document
  (output.txt). It is made up of 4 member functions and a dummy
  constructor.
  """
  def __init__(self):
    print("Document instanciated")

  def parse_contents(self, soup):
    """
    This member function parses the html document and extracts the
    cells from the table.
    """
    data = []
    table = soup.find('table', attrs={'id':'packages'})
    table_body = table.find('tbody')

    rows = table_body.find_all('tr')
    for row in rows:
      cols = row.find_all('td')
      ref = row.find_all('a', href=True)
      cols = [ele.text.strip() for ele in cols]
      relurl = ref[0]['href'].strip('../')
      aburl = urljoin(__target__, relurl)
      cols.append(aburl)
      print(cols)
      data.append(cols)

    return data

  def concat(self, clazz, description, url):
    title = clazz or 'No class found'
    typez = 'A'
    redirect = ''
    four = '' # ignore
    categories = ''
    six = '' # ignore
    related_topics = ''
    eight = '' #ignore
    external_links = ''
    ten = '' # ignore
    image = ''
    abstract = description or "No abstract found"
    url = url or "No URL found"

    data = [
      title,
      typez,
      redirect,
      four,
      categories,
      six,
      related_topics,
      eight,
      external_links,
      ten,
      image,
      abstract,
      url
    ]

    line = "\t".join(data) + "\n"

    return line

  def output(self, data_list):
    """
    The output member function outputs the rows of data to
    """
    f = open('output.txt', 'a')
    for data in data_list:
      line = self.concat(data[0], data[2], data[3])
      f.write(line.encode('utf'))
    f.close()


if __name__ == "__main__":
  print("Starting ", __module__)
  env = Environment()
  doc = Document()
  soup = env.get_contents()
  data = doc.parse_contents(soup)
  doc.output(data)

