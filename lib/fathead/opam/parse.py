#!/usr/bin/python

import os
from urlparse import urljoin

from bs4 import BeautifulSoup

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
    BeautifulSoup object is instanciated, with the contents. More on
    this can be read at the BeautifulSoup documentation website.
    """
    with open('download/packages.html') as f:
      contents = f.read()
    contents = BeautifulSoup(contents, 'html.parser')

    return contents


class Document(object):
  """
  The Document class is responsible for setting up the end document
  (output.txt). It is made up of 4 member functions and a dummy
  constructor.
  """
  def __init__(self):
    print("Document object instanciated")

  def parse_contents(self, soup):
    """
    This member function parses the html document and extracts the
    cells from the table.
    """
    data = []
    table = soup.find('table', attrs={'id':'packages'})
    table_body = table.find('tbody')
    rows = table_body.find_all('tr')

    """
    This section aims to extract the idividual data points from the
    table and turn them into a list. For each of the cols found in
    <td> tags, we return them in a list, while finding one link.
    We then preprocess the contents by stripping whitespace and
    stripping the front of the relative link. Once this preprocessing
    has been done, we connect the url, and append it to the list.
    """
    for row in rows:
      cols = row.find_all('td')
      ref = row.find('a', href=True)

      # Preprocessing
      cols = [ele.text.strip() for ele in cols]
      relurl = ref['href'].strip('../')

      # Add to list
      aburl = urljoin(__target__, relurl)
      cols.append(aburl)
      print(cols)
      data.append(cols)

    return data

  def concat(self, name, description, url):
    """
    The concat (concatanation) member function is responsible for
    for preparing the data to be written to the file. The file is
    layed outlike this as requested in the DuckDuckHack docs found
    here: http://docs.duckduckhack.com/resources/fathead-overview.html#data-file-format
    """
    title      = name or 'No title found'
    typez      = 'A'
    redirect   = ''
    four       = '' # IGNORE
    categories = ''
    six        = '' # IGNORE
    related_topics = ''
    eight      = '' # IGNORE
    external_links = ''
    ten        = '' # IGNORE
    image      = ''
    abstract   = description or "No description found"
    url        = url or "No URL found"

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
    The 'output/1' member function outputs the rows of data at a
    time to the output.txt file which is used as the k/v store
    for the FatHead IA.
    """
    with open('output.txt', 'a') as f:
      for data in data_list:
        line = self.concat(data[0], data[2], data[3])
        f.write(line.encode('utf'))

if __name__ == "__main__":
  print("Launching " +  __module__)
  env = Environment()
  doc = Document()
  contents = env.get_contents()
  processed_contents = doc.parse_contents(contents)
  doc.output(processed_contents)

