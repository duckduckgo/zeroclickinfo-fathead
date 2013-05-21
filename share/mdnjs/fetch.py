import codecs
from httplib2 import urllib
import os
import time

from bs4 import BeautifulSoup

from lxml import etree
    
class CacheFetcher(object):
  """ A wrapper around urllib's fetcher that provides filesystem caching. """
  def __init__(self, cachedir, formatter, sleep=0):
    self._sleep = sleep
    if not os.path.exists(cachedir):
      os.mkdir(cachedir)
    self._cachedir = cachedir
    self._formatter = formatter

  def fetch(self, url):
    """ Fetch url and return a file-like representation. """
    fname = os.path.join(self._cachedir, self._formatter(url))
    if not os.path.exists(fname):
      time.sleep(self._sleep)
      html = urllib.urlopen(url).read()
      with codecs.open(fname, 'w', 'utf-8') as f:
        soup = BeautifulSoup(html)
        f.write(unicode(soup))
    return fname

def extract_sitemap(sitemap, patt):
  """ A generator for crawlable URLs. """
  tree = etree.parse(sitemap).getroot()
  for e in tree.xpath('/x:urlset/x:url/x:loc',
      namespaces={'x': 'http://www.sitemaps.org/schemas/sitemap/0.9'}):
    # `example` is a one-off fix to remove tutorial-like pages.
    # An obvious improvement is to roll this back into pattern.
    if patt in e.text and 'example' not in e.text:
      yield e.text

def filename_formatter(url):
  _, obj, prop = url.rsplit('/', 2)
  return obj + '.' + prop
  
def run(sitemapurl, patt, cachedir, cachejournal, sleep=5):
  """
  Args:
    sitemapurl: A string URL to an XML sitemap.
    patt: A string used for substring matching of the urls in the sitemap.
    cachedir: Directory used to cache downloaded HTML files.
    cachejournal: A string filename to store records about the 
      cache directory. Should be considered a tmp file.
    sleep: Integer amount of time to sleep between HTTP requests, in seconds.
  """
  fetcher = CacheFetcher(cachedir, filename_formatter, sleep)
  sitemap = urllib.urlopen(sitemapurl)
  with open(cachejournal, 'w') as journal:
    for url in extract_sitemap(sitemap, patt):
      fname = fetcher.fetch(url)
      journal.write('{0},{1}\n'.format(fname, url))
  
if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser()
  parser.add_argument('--sitemap')
  parser.add_argument('--patt')
  parser.add_argument('--cachedir')
  parser.add_argument('--cachejournal')
  parser.add_argument('--sleep', type=int)
  args = parser.parse_args()
  run(args.sitemap, args.patt, args.cachedir, args.cachejournal, args.sleep)
