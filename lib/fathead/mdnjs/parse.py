import codecs
import re
import sys
from collections import Counter
from xml.dom.minidom import parse
from lxml import html
from lxml import etree

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
        with codecs.open(specfile, 'r', 'utf-8') as f:
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

    def standardize(self, mdn):
        """ Standardize and clean the fields within an MDN object. """
        if 'Global' in mdn.obj:
            mdn.obj = 'Global'
        if mdn.obj not in self.objects and 'Global' in mdn.url:
            return None
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

    def __init__(self, outfile):
        self.outfile = outfile

    def writerow(self, outdict):
        """ Write the dict row. """
        row = []
        for field in FatWriter.FIELDS:
            col = outdict.get(field, '')
            col = col.replace('\t', '    ')
            col = col.replace('\n', '\\n')
            row.append(col)
        self.outfile.write('\t'.join(row) + '\n')


class MDNWriter(FatWriter):
    """ An implementation of FatWriter that knows how to convert between MDN
        objects and the FatWriter spec. """
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
    summary       Removes the last element from an array and returns that
                  element.
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
    """ A parser that takes an MDN wiki page and returns an MDN object. If
    pages change causing this Fathead to break, then the queries in this class
    should be checked. """
    def _extract_node(self, node):
        if node is not None:
            txt = node.text
            if txt:
                return txt.strip()

    def _is_obsolete(self, tree):
        obsolete = tree.xpath("//meta[@class='obsoleteHeader']")
        return obsolete

    def parse(self, htmlfile):
        """ Parse an html file and return an mdn object.

        Args:
          htmlfile: A file-like object that should parse with beautiful soup's
                    html parser.
        """
        title_el, summary_el, codesnippet_el = None, None, None

        page = htmlfile.read()
        tree = html.fromstring(page)

        if self._is_obsolete(tree):
          print "obsolete"
          return None

        title_el = tree.xpath("//meta[@property='og:title']/@content")
        article = tree.xpath("//meta[@id='wikiArticle']/@content")
        summary_el = tree.xpath("//meta[@property='og:description']/@content")
        if article:
            summary_el = article.find(
              lambda e: e.name == 'p' and e.text.strip() != '',
              recursive=False)
        syntax_header = tree.xpath("//h2[contains(@id,'Syntax')]")
        codesnippet = ""
        if syntax_header:
             elements = tree.xpath("//h2[contains(@id,'Syntax')]/following-sibling::pre[1]")
             for element in elements:
              for tag in element.xpath('//*[@class]'):
                  tag.attrib.pop('class')
              codesnippet += etree.tostring(element, pretty_print=True)

        sys.stdout.write("Parsing %s  " % (title_el[0]) + " " * 30 + "\r")
        sys.stdout.flush()

        mdn = MDN()
        mdn.title = title_el[0]
        if summary_el: mdn.summary = summary_el[0]
        if codesnippet: mdn.codesnippet = codesnippet
        return mdn

class MDNIndexer(object):
    def __init__(self, writer):
        self._writer = writer
        self.counter = Counter()
        self.inverted_index = {}
        # for Web/Api pages
        self.CLASS_WORDS = ['Window', 'Navigator', 'MouseEvent',
                            'KeyboardEvent', 'GlobalEventHandlers', 'Element',
                            'Node', 'Event', 'Selection']

    def add(self, mdn):
        keyword = mdn.prop.lower()
        self.counter[keyword] += 1
        if keyword not in self.inverted_index:
            self.inverted_index[keyword] = []
        self.inverted_index[keyword].append(mdn)

    def writerows(self):
        for keyword, count in self.counter.most_common():
            if count > 1:
                disambig = ''
                for mdn in self.inverted_index[keyword]:
                    if disambig:
                        disambig += '\\n'
                    if '.' in mdn.summary:
                        summary = mdn.summary[:mdn.summary.find('.') + 1]
                    else:
                        summary = mdn.summary
                    disambig += '*[[%s]] %s' % (mdn.title, summary)
                # Write a disambiguation
                self._writer.writerow({
                  'title': keyword,
                  'type': 'D',
                  'disambiguation': disambig
                })
            for mdn in self.inverted_index[keyword]:
                # add redirect for Web/Api pages
                if any(word in mdn.title for word in self.CLASS_WORDS) and '.' in mdn.title:
                    # original title: Window.getAnimationFrame()
                    match = re.search('(?:.*\.)([^\(]+)(?:\(\))?', mdn.title)
                    # remove class_word: getAnimationFrame
                    strip_title = match.group(1)

                    self._writer.writerow({
                      'title': strip_title,
                      'type': 'R',
                      'redirect': mdn.title
                    })
                # For all entries in the inverted index, write a redirect of
                # of the form <object><space><property>
                self._writer.writerow({
                  'title': '%s %s' % (mdn.obj.lower(), mdn.prop.lower()),
                  'type': 'R',
                  'redirect': mdn.title
                })
                # If this is the only item in the inverted index,
                # write a primary redirect on the keyword.
                if count == 1:
                    self._writer.writerow({
                      'title': keyword,
                      'type': 'R',
                      'redirect': mdn.title
                    })


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
    journal = [l.strip().split(',') for l in
               open(cachejournal).read().splitlines()]
    with codecs.open(outfname, 'w', 'utf-8') as outfile:
        writer = MDNWriter(outfile)
        indexer = MDNIndexer(writer)
        # Iterate over URLs in the sitemap ...
        for fname, url in journal:
            # ... and parse each to generate an mdn object.
            mdn = parser.parse(codecs.open(fname, 'r', 'utf-8'))
            if not mdn or not mdn.summary:
                continue
            # WARNING WARNING
            #
            #  If MDN updates their URL structure, this will break. This
            #  assumes that the URL ends with /obj/property
            #
            #  An improvement would be to supply this as a regex pattern
            #  to the CL
            #
            # WARNING WARNING
            _, obj, prop = url.rsplit('/', 2)
            mdn.url = url
            mdn.obj = obj
            mdn.prop = prop
            mdn = standardizer.standardize(mdn)
            if mdn is None:
                continue
            # Here we require that outputs have either a summary
            # or a code sample.
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
