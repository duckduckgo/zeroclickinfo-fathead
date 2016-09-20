import codecs
import re
from collections import Counter
from lxml import html, etree

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
        self.articles_index = []

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
        if mdn.exampledesc:
            abstract += '\n' + mdn.exampledesc
        if mdn.example:
            code = '<pre><code>%s</code></pre>' % mdn.example
            abstract += '\n' + code
        d = {
          'title': mdn.title,
          'type': 'A',
          'source_url': mdn.url,
          'abstract': abstract
        }
        self.writerow(d)
        self.articles_index.append(mdn.title.lower())


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
      articletype: Article categories (eg. Functions, Error)
      exampledesc: Example Description
      example: Example Code
      redirected: To control the addition of repeatitive redirections from the same object to output.txt
    """
    def __init__(self, title=None, url=None, summary=None, codesnippet=None,
                 obj=None, prop=None, articletype=None):
        self.title = title
        self.url = url
        self.summary = summary
        self.codesnippet = codesnippet
        self.obj = obj
        self.prop = prop
        self.articletype = articletype
        self.exampledesc = None
        self.example = None
        self.redirected = False

class MDNParser(object):
    """ A parser that takes an MDN wiki page and returns an MDN object. If
    pages change causing this Fathead to break, then the queries in this class
    should be checked. """
    def _is_obsolete(self, tree):
        obsolete = tree.xpath("//*[contains(@class, 'obsoleteHeader')]")
        return obsolete

    def parse(self, htmlfile):
        """ Parse an html file and return an mdn object.

        Args:
          htmlfile: A file-like object that should parse with lxml html parser.
        """
        page = htmlfile.read()
        tree = html.fromstring(page)

        if self._is_obsolete(tree):
          return None

        title = tree.xpath("//meta[@property='og:title']/@content")[0]
        article = tree.xpath("//article[contains(@id,'wikiArticle')]")
        summary = ""
        if article:
            summary_nodes = tree.xpath("//h2[contains(@id,'Summary')]/following-sibling::p[1]")
            for summary_el in summary_nodes :
              for tag in summary_el.xpath('//*[@class]'):
                  tag.attrib.pop('class')
              summary += re.sub('<[^<]+?>', '', etree.tostring(summary_el).strip())
        
        # if there's no summary, getting description section
        if not summary:
            summary_el = tree.xpath("//meta[@property='og:description']/@content")
            if summary_el:
              summary = summary_el[0]

        # if there's no summary or description, getting see also section title
        if not summary:
            see_also_el = tree.xpath("//h3[contains(@id,'See_also')]")
            if see_also_el:
                elements = tree.xpath("//h3[contains(@id,'See_also')]/following-sibling::ul[1]")
                for element in elements:
                    for tag in element.xpath('//*[@class]'):
                        tag.attrib.pop('class')
                    summary = re.findall('title="([^"]*)"', etree.tostring(element).strip())
                summary = summary[0].strip()

        codesnippet = ""
        syntax_header = tree.xpath("//h2[contains(@id,'Syntax')]")
        if syntax_header:
            elements = tree.xpath("//h2[contains(@id,'Syntax')]/following-sibling::pre[1]")
            for element in elements:
                for tag in element.xpath('//*[@class]'):
                    tag.attrib.pop('class')
                codesnippet += re.sub('<[^<]+?>', '', etree.tostring(element).strip())

        articletype = ""
        exampledesc = ""
        example = ""
        # Error pages
        if "Error" in htmlfile.name:

          articletype = "Error"

          # What went wrong?
          whatWentWrong_summary = ""
          whatWentWrong = tree.xpath("//h2[contains(@id,'What_went_wrong')]/following-sibling::p[1]")
          for element in whatWentWrong:
              for tag in element.xpath('//*[@class]'):
                  tag.attrib.pop('class')
              whatWentWrong_summary += re.sub('<[^<]+?>', '', etree.tostring(element).strip())

          if whatWentWrong_summary:
            summary = whatWentWrong_summary

          # Examples
          exampleGood = ''.join(tree.xpath("//h3[contains(@id,'Valid_cases')]/following-sibling::pre/text()"))
          exampleBad = ''.join(tree.xpath("//h3[contains(@id,'Invalid_cases')]/following-sibling::pre/text()"))

          exampleGood = re.sub('<[^<]+?>', '', exampleGood)
          exampleBad = re.sub('<[^<]+?>', '', exampleBad)

          if exampleGood:
            exampleGood = "Valid Cases:\n" + exampleGood
          if exampleBad:
            exampleBad = "Invalid Cases:\n" + exampleGood

          if exampleBad or exampleGood:
            codesnippet = exampleBad + "\n" + exampleGood
            
        if any(wiki in htmlfile.name for wiki in ["Functions.", "Classes.", "Statements."]):
            
            articletype = htmlfile.name.split('.')[0].split('/')[1]
            desc_header = tree.xpath("//h2[contains(@id,'Description')]")

            if desc_header:

                elements = tree.xpath("//h2[contains(@id,'Description')]/following-sibling::p[1]")
                
                for element in elements:
                    for tag in element.xpath('//*[@class]'):
                        tag.attrib.pop('class')
                    exampledesc += re.sub('<[^<]+?>', '', etree.tostring(element).strip())
                        
                elements = tree.xpath("//h2[contains(@id,'Description')]/following-sibling::pre[1]")
                
                for element in elements:
                    for tag in element.xpath('//*[@class]'):
                        tag.attrib.pop('class')
                    example += re.sub('<[^<]+?>', '', etree.tostring(element).strip())
            else:
                
                elements = tree.xpath("//h2[contains(@id,'Examples')]/following-sibling::p[1]")
                
                for element in elements:
                    for tag in element.xpath('//*[@class]'):
                        tag.attrib.pop('class')
                    exampledesc += re.sub('<[^<]+?>', '', etree.tostring(element).strip())
                        
                elements = tree.xpath("//h2[contains(@id,'Examples')]/following-sibling::pre[1]")
                
                for element in elements:
                    for tag in element.xpath('//*[@class]'):
                        tag.attrib.pop('class')
                    example += re.sub('<[^<]+?>', '', etree.tostring(element).strip())
                        
        print title + (' ' * 30) + '\r',

        mdn = MDN()
        mdn.title = title
        mdn.summary = summary
        mdn.codesnippet = codesnippet
        mdn.exampledesc = exampledesc
        mdn.example = example
        mdn.articletype = articletype
        return mdn

class MDNIndexer(object):
    def __init__(self, writer):
        self._writer = writer
        self.counter = Counter()
        self.inverted_index = {}
        # for Web/Api pages
        self.WEBAPI_CLASS_WORDS = ['Window', 'Navigator', 'MouseEvent',
                            'KeyboardEvent', 'GlobalEventHandlers', 'Element',
                            'Node', 'Event', 'Selection']
        # for Error pages
        self.ERROR_SYNONYMS = [ ["bad", "not legal", "invalid", "not a valid"] ]
        # for syntax, example redirections
        self.SYTAX_EXAMPLE_REDIR = ["Functions", "Classes", "Statements"]
        self.EXCEPTIONS = ["if", "else", "each", "method"]

    def add(self, mdn):
        keyword = mdn.prop.lower()
        self.counter[keyword] += 1
        if keyword not in self.inverted_index:
            self.inverted_index[keyword] = []
        self.inverted_index[keyword].append(mdn)

    def writestrippedredirect(self, mdn):
        #lowercase title and strip quotation marks ("), color(:) and period(.) between two words
        strippedRedirect = re.sub('(?<=[a-z])([.])(?!\d)', ' ', mdn.title.replace(',', ' ').replace(':',' ').replace('"', ' ').lower())
        strippedRedirect = re.sub( '\s+', ' ',strippedRedirect ).strip()
        if (strippedRedirect != mdn.title.lower()):
            self._writer.writerow({
              'title': strippedRedirect,
              'type': 'R',
              'redirect': mdn.title
            })

    def writeredirect(self, title, mdn):
        title = title.replace('_', ' ')
        title = title.split('...')
        title = ' '.join(title)
        # write redirects with synomyms for error pages
        if mdn.articletype == "Error":
            for synonyms_list in self.ERROR_SYNONYMS:
                if any(word in title.lower() for word in synonyms_list): 
                    word = set(synonyms_list).intersection(title.lower().split()).pop()
                    for synonym in synonyms_list:
                        self._writer.writerow({
                            'title': title.replace(word, synonym),
                            'type': 'R',
                            'redirect': mdn.title
                        })
                    return;
        # write redirects with `syntax` and `example` for functions pages
        if any(wiki == mdn.articletype for wiki in self.SYTAX_EXAMPLE_REDIR) and not mdn.redirected:
            mdn.redirected = True
            self._writer.writerow({
                'title': title + " syntax",
                'type': 'R',
                'redirect': mdn.title
            })
            self._writer.writerow({
                'title': title + " example",
                'type': 'R',
                'redirect': mdn.title
            })
            split_title = title.split(' ')
            if any(split_title[0].lower() == wiki.lower() for wiki in self.SYTAX_EXAMPLE_REDIR) and len(split_title) > 1:
                new_title = split_title[1:]
                new_title = ' '.join(new_title).lower()
                if new_title != mdn.title.lower():
                    self._writer.writerow({
                        'title': new_title,
                        'type': 'R',
                        'redirect': mdn.title
                    })
                self._writer.writerow({
                    'title': new_title + " syntax",
                    'type': 'R',
                    'redirect': mdn.title
                })
                self._writer.writerow({
                    'title': new_title + " example",
                    'type': 'R',
                    'redirect': mdn.title
                })
                new_title = new_title.split(' ')
                for split_title in new_title:
                    if any(exceptions == split_title for exceptions in self.EXCEPTIONS):
                        self._writer.writerow({
                            'title': split_title,
                            'type': 'R',
                            'redirect': mdn.title
                        })
                        self._writer.writerow({
                            'title': split_title + " syntax",
                            'type': 'R',
                            'redirect': mdn.title
                        })
                        self._writer.writerow({
                            'title': split_title + " example",
                            'type': 'R',
                            'redirect': mdn.title
                        })
            
        # To avoid redirections like "default parameters" -> "Default Parameters"
        if title.lower() != mdn.title.lower():
            self._writer.writerow({
                'title': title,
                'type': 'R',
                'redirect': mdn.title
            })

    def writedisambiguation(self, keyword, disambig):
        self._writer.writerow({
          'title': keyword,
          'type': 'D',
          'disambiguation': disambig
        })
        self._writer.articles_index.append(keyword.lower())

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
                # skips D if already an article
                if keyword.lower() not in self._writer.articles_index:
                    self.writedisambiguation(keyword, disambig)
                    
            for mdn in self.inverted_index[keyword]:
                # add redirect for Web/Api pages
                strip_title = ""
                if any(word in mdn.title for word in self.WEBAPI_CLASS_WORDS) and '.' in mdn.title:
                    # original title: Window.getAnimationFrame()
                    match = re.search('(?:.*\.)([^\(]+)(?:\(\))?', mdn.title)
                    # remove class_word: getAnimationFrame
                    strip_title = match.group(1)
                    # skips redirect if already an article
                    if strip_title.lower() not in self._writer.articles_index:
                        self.writeredirect(strip_title, mdn)
                # for all entries in the inverted index, write a redirect of
                # of the form <object><space><property>
                obj_prop_entry = mdn.obj.lower() + ' ' + mdn.prop.lower()
                if obj_prop_entry not in self._writer.articles_index:
                    self.writeredirect(obj_prop_entry, mdn)
                # If this is the only item in the inverted index,
                # write a primary redirect on the keyword.
                if count == 1:
                    # check if not an Article
                    if not all(x in [keyword, strip_title] for x in self._writer.articles_index):
                        if keyword.lower() not in self._writer.articles_index:
                            self.writeredirect(keyword, mdn)

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
                # For the error articles, we write a redirect
                # with a stripped version of the article
                if mdn.articletype == "Error":            
                    indexer.writestrippedredirect(mdn)
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