# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import re
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()

URL_ROOT = "https://pika.readthedocs.io/en/latest/"
MODULES_URL_SUFFIX = {
    'BlockingConnection' : 'modules/adapters/blocking.html',
    'Select Connection Adapter' : 'modules/adapters/select.html',
    'Tornado Connection Adapter' : 'modules/adapters/tornado.html',
    'Twisted Connection Adapter' : 'modules/adapters/twisted.html',
    'Channel' : 'modules/channel.html',
    'Connection' : 'modules/connection.html',
    'Authentication Credentials' : 'modules/credentials.html',
    'Exceptions' : 'modules/exceptions.html',
    'Connection Parameters' : 'modules/parameters.html',
    'pika.spec' : 'spec.html'
}


def replace_all(text, terms):
    """
    Replace all the terms contained
    in a dict {'from':'to'}.
    """
    for _from, _to in terms.items():
        text = text.replace(_from, _to)

    return text


def format_signature(signature):
    """
    Remove unwanted characters from the
    signature of various classes and methods.
    """
    terms = {
        '\n' : '',
        '[source]' : '',
        u'¶' : ''
    }

    return replace_all(signature, terms)


def build_abstract(info, example=None, detailed_info={}):
    """
    Constructs the abstract of an entry according to
    the new fathead HTML template elements.
    """
    abstract = ''
    if info:
        abstract += '<p>%s</p>' % info.replace('\n', '\\n')

    if example:
        abstract += '<pre><code>%s</code></pre>' % example.replace('\n', '\\n')

    for key, value in detailed_info.items():
        abstract += '<span class="prog__sub">%s</span><p>%s</p>' % (key, value)

    abstract = '<section class="prog__container">%s</section>' % abstract.replace('\n', '\\n')
    return abstract


def remove_excess_padding(text):
    """
    Remove Excess padding like extra
    '\n's around the given text
    """
    text = re.sub('^\\n', '', text)
    text = re.sub('\\n$', '', text)

    return text


class Class(object):
    """
    Holds the information about the various
    classes present in the documentation
    """

    def __init__(self, data):
        self.data = data
        self.output = []

    def parse_data(self):
        # Create an Entry for the class.

        self.class_name = self.data.dt.find('code', {'class':'descname'}).text
        
        logger.info('Parsing Class : %s' % self.class_name)
        
        class_signature = self.data.dt.text
        url_tag = self.data.dt.find('a', {'class':'headerlink'})
        class_url = url_tag.get('href') if url_tag else ''
        class_info = self.data.dd.p.text if self.data.dd.p else ''
        
        class_signature = format_signature(class_signature)
        class_abstract = build_abstract(info=class_info, example=class_signature)

        entry = {
        'title' : self.class_name,
        'type' : 'A',
        'abstract' : class_abstract,
        'anchor' : class_url
        }

        self.output.append(entry)

        #check for existence of sub-classes.
        
        sub_classes = self.data.dd.findAll('dl', {'class':'class'}, recursive=False)
        for sub_class in sub_classes:
            cls = Class(sub_class)
            cls.parse_data()
            sub_cls_output = cls.get_output()
            for entry in sub_cls_output:
                entry['title'] = "%s.%s" % (self.class_name, entry['title'])

            self.output += sub_cls_output
        #create Entries for methods and attributes of the class.

        methods = self.data.dd.findAll('dl', {'class':['method', 'attribute']}, recursive=False)
        for method in methods:
            # Ignore Elements with empty descriptions
            if not method.dd.text:
                continue

            method_name = method.dt.find('code', {'class':'descname'}).text
            method_signature = method.dt.text
            url_tag = method.dt.find('a', {'class':'headerlink'})
            method_url = url_tag.get('href') if url_tag else ''
            method_info = method.dd.p.text if method.dd.p else ''

            logger.info('Parsing Method : %s' % method_name)

            method_desc = method.dd.tbody
            method_detailed_info = {}

            if method_desc:
                rows = method_desc.findAll('tr')
                for row in rows:
                    key = row.th.text
                    value = remove_excess_padding(row.td.text)
                    method_detailed_info[key] = value

            method_name = "%s.%s" % (self.class_name, method_name)
            method_signature = format_signature(method_signature)
            method_abstract = build_abstract(info=method_info,
                                             example=method_signature,
                                             detailed_info=method_detailed_info)

            entry = {
            'title' : method_name,
            'type' : 'A',
            'abstract' : method_abstract,
            'anchor' : method_url
            }

            self.output.append(entry)
        
        logger.info('Finished Parsing Class : %s' % self.class_name)

    def get_output(self):
        return self.output


class Module(object):
    """
    Holds the information about the various
    modules present in the documentation.
    """

    def __init__(self, name, soup):
        self.name = name
        self.url = URL_ROOT + MODULES_URL_SUFFIX.get(name, '')
        self.soup = soup

    def parse_for_classes(self):
        self.classes = []
        cls_descriptions = self.soup.findAll('dl', {'class':['class', 'exception']}, recursive=False)

        # if no classes found.
        if not cls_descriptions:
            cls_descriptions = []
            sections = self.soup.findAll('div', {'class':'section'}, recursive=False)
            for section in sections:
                cls_descriptions += section.findAll('dl', {'class':'class'}, recursive=False)

        for cls_desc in cls_descriptions:
            cls = Class(cls_desc)
            self.classes.append(cls)

    def get_output(self):
        output = []

        for cls in self.classes:
            cls.parse_data()
            cls_output = cls.get_output()
            for entry in cls_output:
                entry['categories'] = self.name
                entry['external_link'] = URL_ROOT
                entry['anchor'] = "%s%s" % (self.url, entry['anchor'])
            output += cls_output

        return output


class Parser(object):
    """
    Parses the HTML file to get
    relavent modules present in it.
    """

    def __init__(self, input='download/pika-latest/index.html'):
        doc_id = 'core-class-and-module-documentation'
        soup = BeautifulSoup(open(input), 'html.parser', from_encoding='utf-8')
        soup = soup.find('div', attrs={'id':doc_id}).extract()

        self.modules = []
        self.soup = soup

    def parse_for_modules(self, soup=None):
        if soup is None:
            soup = self.soup

        doc_content_class = 'toctree-wrapper compound'
        soup = soup.find('div', attrs={'class':doc_content_class}).extract()
        module_sections = soup.findAll('div', {'class':'section'}, recursive=False)

        for section in module_sections:
            if section.find('div', attrs={'class':doc_content_class}):
                self.parse_for_modules(section)
            else:
                module_name = replace_all(section.find(['h4', 'h6']).text, {u'¶':''})
                module = Module(module_name, section)
                self.modules.append(module)


class FatWriter(object):
    """
    Writes Parsed data to the output
    file in the desired format.
    """
    
    def __init__(self, data):
        self.data = data

    def format_line(self, line):
        fields = [
            'title',
            'type',
            'redirect',
            'ignore',
            'categories',
            'ignore',
            'related_topics',
            'ignore',
            'external_link',
            'disambiguation',
            'image',
            'abstract',
            'anchor'
        ]

        formatted_line = []
        for field in fields:
            formatted_line.append(line.get(field, ''))

        return formatted_line

    def create_file(self):
        with open('output.txt', 'w+') as output_file:
            for line in self.data:
                formatted_line = self.format_line(line)
                tsv = '\t'.join(formatted_line).encode('utf-8')
                output_file.write(tsv + '\n')

if __name__ == '__main__':
    logger.info('Initializing Parser and Parsing For Modules')
    parser = Parser()
    parser.parse_for_modules()

    output = [] 
    for module in parser.modules:
        logger.info('Parsing Module : %s' % module.name)
        module.parse_for_classes()
        output += module.get_output()
        logger.info('Finished Parsing Module : %s' % module.name)

    writer = FatWriter(output)
    writer.create_file()
