# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
from os import listdir
import cgi
import logging
import re

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()

def replace_all(text, terms):
    """ Replaces all terms contained
    in a dict """
    for _from, _to in terms.items():
        text = text.replace(_from, _to)
    return text


class Tag(object):
    """ Contains informations about
    a HTML tag """
    def __init__(self, name, info, reference, example, see_also):
        self.name = name
        self.info = info
        self.reference = reference
        self.see_also = see_also

        # Remove excess padding around code snippet
        self.example = re.sub('^\\n', '', example)
        self.example = re.sub('\\n$', '', self.example)

        # replace all newline and horizontal tab characters
        terms = {'\n': '\\\\n', '\t': '\\\\t', '\r': ''}
        self.info = cgi.escape(replace_all(self.info, terms))
        self.example = cgi.escape(replace_all(self.example, terms))

    def make_abstract(self):
        """ Creates the abstract using
        the new fathead template elements """
        abstract = ''

        if self.example:
            abstract += '<pre><code>' + self.example + '</pre></code>'

        if self.info:
            abstract += '<p>' + self.info + '</p>'

        abstract = '<section class="prog__container">' + abstract + '</section>'

        return abstract

    def __str__(self):
        fields = [
                self.name,                  # Title
                'A',                        # Article Type
                '',                         # No Redirect
                '',                         # Ignore
                '',                         # Categories
                '',                         # Ignore
                '\\\\n'.join(self.see_also),# Related Topics
                '',                         # Ignore
                '',                         # External Links
                '',                         # For Disambiguation Pages only
                '',                         # Image
                self.make_abstract(),       # Abstract
                self.reference              # URL
                ]

        output = '%s' % ('\t'.join(fields))

        return output


class Parser(object):
    """ Parses a HTML file to get
    all tag informations inside it """
    def __init__(self, input='download/'):
        self.base_dir = input
        self.files = listdir(input)

    def get_tags(self):
        """ Gets all tags defined in 'dl' tags """
        self.tags = []
        for file in self.files:
            name = file
            file_contents = open(self.base_dir + name).read()
            soup = BeautifulSoup(file_contents, 'html5lib')
            article = soup.article

            # getting info about tag
            info = ''
            for p in article.find_all('p', recursive = False):
                text = p.getText()
                if text:
                    info = text
                    break

            if not info:
                info = soup.find('meta', {'property' : 'og:description'}).get('content')

            # getting code snippet
            example = ''
            pre_tag = article.pre
            if pre_tag:
                example = pre_tag.getText()

            # getting the see also tags.
            see_also = []
            see_also_heading = article.find('h2', {'id' : 'See_also'})
            if see_also_heading and see_also_heading.find_next('ul'):
                for li in see_also_heading.find_next('ul').find_all('li'):
                    for a_tag in li.find_all('a'):
                        href = a_tag['href']
                        lang = href.split('/')[-3]
                        if lang == "HTML":
                            elem_name = href.split('/')[-1]
                            see_also.append("[[" + elem_name + "]]")

            # getting reference link for the tag
            reference = 'https://developer.mozilla.org/en-US/docs/Web/HTML/Element/' + name
            new_tag = Tag(name, info, reference, example, see_also)
            self.tags.append(new_tag)
            logger.info('Tag parsed: %s' % new_tag.name)


if __name__ == '__main__':
    parser = Parser()
    parser.get_tags()

    with open('output.txt', 'w') as file:
        for tag in parser.tags:
            file.write(tag.__str__().encode('utf-8') + '\n')
            logger.info('Tag added to output: %s' % tag.name)
