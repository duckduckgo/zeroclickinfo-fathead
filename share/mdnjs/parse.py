from collections import Counter
import cgi
import csv

from lxml.html import parse
from unidecode import unidecode

class Standardizer(object):
  """ Standardize the titles of each entry.

  MDN uses a wiki for its documentation, so titles aren't consistent.
  For example, you might have:
    Array reverse
    Array.reverse
    Array.prototype.reverse
  """
  TITLE_FORMATTING = {
    'class_property': '%s.%s',
    'class_function': '%s.%s',
    'instance_method': '%s.prototype.%s',
    'instance_property': '%s.prototype.%s',
  }
  def __init__(self, specfile):
    """
    Args:
      specfile: A filesystem path to a csv file containing language 
      definitions. It should have the format:
        BaseObject,property,{class_property,class_function,instance_method,instance_property}
    """
    self.inverted_index = {}
    self.objects = set()
    with open(specfile) as f:
      for line in f:
        line = line.strip()
        index = line.split('(')[0]
        if index.count('.') > 1:
          index = index.split('prototype')[-1]
        index = index.split('.')[-1].lower().strip()
        if index not in self.inverted_index:
          self.inverted_index[index] = []
        self.inverted_index[index].append(line)

        obj = line.split('.')[0]
        self.objects.add(obj)
    print self.inverted_index['prototype']

  def standardize(self, mdn):
    """ Standardize and clean the fields within an MDN object. """
    if 'Global' in mdn.obj: 
      mdn.obj = 'Global'
    if mdn.obj not in self.objects:
      return None
    if mdn.prop.lower() == 'length':
      print mdn.prop, mdn.obj
    if mdn.prop.lower() not in self.inverted_index:
      return mdn
    for signature in self.inverted_index[mdn.prop.lower()]:
      if signature.startswith(mdn.obj):
        mdn.codesnippet = signature
        mdn.title = signature.split('(')[0].strip()
        break

    return mdn


class FatWriter(object):
  """ File writer for DDG Fathead files. Field orders and output format
  comply with the documentation at https://github.com/duckduckgo/
  zeroclickinfo-fathead."""

  FIELDS = [
    'title',
    'type',
    'redirect',
    'otheruses',
    'categories',
    'references',
    'see_also',
    'further_reading',
    'external_links',
    'disambiguation',
    'images',
    'abstract',
    'source_url'
  ]
  def __init__(self, csvfile):
    self._writer = csv.DictWriter(csvfile, FatWriter.FIELDS, delimiter="\t")
    self.fields = set()

  def writerow(self, row):
    """ Write the dict row. """
    self._writer.writerow(row)

class MDNWriter(FatWriter):
  """ An implementation of FatWriter that knows how to convert between MDN objects
      and the FatWriter spec. """
  def writemdn(self, mdn):
    code = ''
    abstract = ''
    if mdn.codesnippet:
      code = '<pre><code>%s</code></pre>' % mdn.codesnippet
    if mdn.summary:
      if abstract:
        abstract += ': '
      abstract += mdn.summary
    abstract = code + abstract
    d = {
      'title': mdn.title,
      'type': 'A', 
      'source_url': mdn.url,
      'abstract': abstract 
    }
    self.writerow(d)

class MDN(object):
  """ A container object for an MDN article. 

  For example, given http://developer.mozilla.org/en-US/docs/
  JavaScript/Reference/Global_Objects/Array/pop, the object would have these
  properties:

  title         Array.pop
  url           http://developer.mozilla.org ...
  summary       Removes the last element from an array and returns that element.
  codesnippet   array.pop()
  obj           Array
  prop          pop

  
  Args:
    title: The article's title.
    url: The articles full URL.
    summary: A couple-sentence overview of the article.
    codesnippet: A couple lines of code showing the syntax. Multiple lines
    should be delimited with \n.
    obj: The calling object.
    prop: The calling object's property.
  """
  def __init__(self, title=None, url=None, summary=None, codesnippet=None,
                     obj=None, prop=None):
    self.title = title
    self.url = url
    self.summary = summary
    self.codesnippet = codesnippet
    self.obj = obj
    self.prop = prop
        
class MDNParser(object):
  """ A parser that takes an MDN wiki page and returns an MDN object. If pages change
  causing this Fathead to break, then the xpaths in this class should be checked. """
  def _extract_node(self, nodelist):
    """ Pop the `first` item from the nodelist, and return its text content. """
    if len(nodelist) < 1:
      return None
    txt = nodelist[0].text_content()
    if txt:
      return cgi.escape(unidecode(txt.strip())).replace('\n','\\n')

  def _is_obsolete(self, tree):
    obsolete = tree.xpath('//*[contains(@class, "obsoleteHeader")]')
    return len(obsolete) > 0

  def parse(self, htmlfile):
    """ Parse an html file and return an mdn object.

    Args:
      htmlfile: A file-like object that should parse with lxml's html parser.
    """
    doc = parse(htmlfile).getroot()
    if self._is_obsolete(doc):
      return None
    title_els = doc.xpath('//*[@id="article-head"]/div/h1')
    summary_els = doc.xpath(
      '//*[(self::h2 or self::h3) and contains(text(), "Summary")]/following-sibling::*[1]')
    if len(summary_els) == 0:
      summary_els = doc.xpath('//*[@id="wikiArticle"]/p[1]')
    codesnippet_els = doc.xpath(
      '//*[(self::h2 or self::h3) and contains(text(), "Syntax")]/following-sibling::*[1]')
    mdn = MDN()
    mdn.title = self._extract_node(title_els)
    mdn.summary = self._extract_node(summary_els)
    mdn.codesnippet = self._extract_node(codesnippet_els)
    return mdn

class MDNIndexer(object):
  def __init__(self, writer):
    self._writer = writer
    self.counter = Counter()
    self.inverted_index = {}

  def add(self, mdn): 
    keyword = mdn.prop.lower()
    self.counter[keyword] += 1
    if keyword not in self.inverted_index:
      self.inverted_index[keyword] = []
    self.inverted_index[keyword].append(mdn)

  def writerows(self):
    for keyword, count in self.counter.most_common():
      if count == 1:
        # Write a redirect
        d = {
          'title': keyword,
          'type': 'R',
          'redirect': self.inverted_index[keyword][0].title
        }
      if count > 1:
        disambig = ''
        for mdn in self.inverted_index[keyword]:
          if disambig:
            disambig += '\\n'
          if '.' in mdn.summary:
            summary = mdn.summary[:mdn.summary.find('.') + 1]
          else:
            summary = mdn.summary
          disambig += '[[%s]] %s' % (mdn.title, summary)
        d = {
          'title': keyword,
          'type': 'D',
          'disambiguation': disambig
        }
        # Write a disambiguation
        pass
      self._writer.writerow(d)

def run(cachedir, cachejournal, langdefs, outfname):
  """
  Args:
    cachedir: Directory used to cache downloaded HTML files.
    cachejournal: A csv of fname,url pairs for the cache dir. 
    langdefs: A filepath to a language definition for JavaScript. See
      the Standardizer class for info on this spec.
    outname: The output filename.
  """
  standardizer = Standardizer(langdefs)
  parser = MDNParser()
  journal = [l.strip().split(',') for l in open(cachejournal).read().splitlines()]
  with open(outfname, 'w') as outfile:
    writer = MDNWriter(outfile)
    indexer = MDNIndexer(writer)
    # Iterate over URLs in the sitemap ...
    for fname, url in journal:
      # ... and parse each to generate an mdn object.
      mdn = parser.parse(open(fname))
      if not mdn:
        continue
      # WARNING WARNING
      # 
      #  If MDN updates their URL structure, this will break. This assumes that
      #  the URL ends with /obj/property
      #
      #  An improvement would be to supply this as a regex pattern to the CL
      #
      # WARNING WARNING
      _, obj, prop = url.rsplit('/', 2)
      mdn.url = url
      mdn.obj = obj
      mdn.prop = prop
      mdn = standardizer.standardize(mdn)
      if mdn is None:
        continue
      # Here we require that outputs have either a summary or a code sample.
      if mdn.summary or mdn.codesnippet:
        writer.writemdn(mdn)
        indexer.add(mdn)
    indexer.writerows()
  
if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser()
  parser.add_argument('--out')
  parser.add_argument('--cachedir')
  parser.add_argument('--cachejournal')
  parser.add_argument('--langdefs')
  args = parser.parse_args()
  run(args.cachedir, args.cachejournal, args.langdefs, args.out)
