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
    self.d = {}
    with open(specfile) as f:
      for line in f:
        obj, prop, typ = line.strip().split(',')
        if obj not in self.d:
          self.d[obj] = {}
        self.d[obj][prop] = typ

  def _title(self, obj, prop):
    typ = self.d.get(obj, {}).get(prop)
    if typ is None:
      prop = prop[:1].lower() + prop[1:]
      typ = self.d.get(obj, {}).get(prop)
    if typ is None or typ not in Standardizer.TITLE_FORMATTING:
      return
    fmt = Standardizer.TITLE_FORMATTING[typ]
    return fmt %(obj, prop)

  def standardize(self, mdn):
    """ Standardize and clean the fields within an MDN object. """
    title = self._title(mdn.obj, mdn.prop)
    if title:
      mdn.title = title
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
  def __init__(self, csvfile, unique=False):
    self._writer = csv.DictWriter(csvfile, FatWriter.FIELDS, delimiter="\t")
    self.unique = True
    self.fields = set()

  def writerow(self, row):
    """ Write the dict row. """
    if self.unique:
      if row.get('title') in self.fields:
        return
      self.fields.add(row.get('title'))
    self._writer.writerow(row)

class MDNWriter(FatWriter):
  """ An implementation of FatWriter that knows how to convert between MDN objects
      and the FatWriter spec. """
  def writerow(self, mdn):
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
    super(MDNWriter, self).writerow(d)

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

def run(cachedir, cachejournal, langdefs, outfname):
  """
  Args:
    cachedir: Directory used to cache downloaded HTML files.
    cachejournal: A csv of fname,url pairs for the cache dir. 
    langdefs: A filepath to a language definition for JavaScript. See
      the Standardizer class for info on this spec.
    outname: The output filename.
  """
  standardizer = Standardizer('propref.csv')
  parser = MDNParser()
  journal = [l.strip().split(',') for l in open(cachejournal).read().splitlines()]
  with open(outfname, 'w') as outfile:
    writer = MDNWriter(outfile, unique=True)
    # Iterate over URLs in the sitemap ...
    for fname, url in journal:
      # ... and parse each to generate an mdn object.
      mdn = parser.parse(open(fname))
      if not mdn:
        continue
      # The MDN urls have the format BASE/obj/prop, where
      # BASE is some arbitrary wiki base. For example, 
      # https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/String/length
      _, obj, prop = url.rsplit('/', 2)
      mdn.url = url
      mdn.obj = obj
      mdn.prop = prop
      mdn = standardizer.standardize(mdn)
      # Here we require that outputs have either a summary or a code sample.
      if mdn.summary or mdn.codesnippet:
        writer.writerow(mdn)
  
if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser()
  parser.add_argument('--out')
  parser.add_argument('--cachedir')
  parser.add_argument('--cachejournal')
  parser.add_argument('--langdefs')
  args = parser.parse_args()
  run(args.cachedir, args.cachejournal, args.langdefs, args.out)
