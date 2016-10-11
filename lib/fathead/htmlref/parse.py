# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import logging
import cgi
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
    def __init__(self, name, info, reference, example):
        self.name = name
        self.info = info
        self.reference = reference
        
        # Remove excess padding around synopsis
        self.example = re.sub('^\\n', '', example)
        self.example = re.sub('\\n$', '', self.example)

        self.example = replace_all(self.example, {'\n': '\\n',
                                             '\t': '\\t',
                                             '\r': ''})

    def __str__(self):
        fields = [
                self.name,                                                       # unique name
                'A',                                                             # type is article
                '',                                                              # no redirect data
                '',                                                              # ignore
                '',                                                              # no categories
                '',                                                              # ignore
                '',                                                              # no related topics
                '',                                                              # ignore
                'http://html5doctor.com/element-index',                          # add an external link back to html home
                '',                                                              # no disambiguation
                '',                                                              # images
                '<pre><code>' + self.example + '</code></pre><br>' + self.info,  # abstract
                self.reference                                                   # url to doc
                ]

        output = '%s' % ('\t'.join(fields))

        return output


class Parser(object):
    """ Parses a HTML file to get
    all tag informations inside it """
    def __init__(self, input='download/index.html'):
        self.soup = BeautifulSoup(open(input), from_encoding='utf-8')

    def get_tags(self):
        """ Gets all tags defined in 'dl' tags """
        self.tags = []
        for tag in self.soup.find_all('dl'):
            name = tag.dt.contents[0]

            # getting info about tag
            info = ''
            for p in tag.dd.find_all('p'):
                info += p.getText() + ' '

            # getting reference link and code snippet
            a_tags = tag.dd.find_all('a')
            example_id = a_tags[1]['href'].replace('#', '')  # code snippet
            example = self.soup.find('div', {'id': example_id}).getText()

            # url reference (from HTML5Doctor if exists)
            reference = ''
            try:
                reference = tag.dt.span.a['href']  # url for HTML5Doctor
            except:
                reference = a_tags[0]['href']  # url for W3C

            reference = 'http://html5doctor.com/element-index/#' + name

            # H1 - H6
            if name == 'h1-h6':
                for n in range(1, 7):
                    new_tag = Tag('h' + str(n), info, reference, example)
                    self.tags.append(new_tag)
            else:
                new_tag = Tag(name, info, reference, example)
                self.tags.append(new_tag)

            logger.info('Tag parsed: %s' % new_tag.name)


if __name__ == '__main__':
    parser = Parser()
    parser.get_tags()

    with open('output.txt', 'w') as file:
        for tag in parser.tags:
            file.write(tag.__str__().encode('utf-8') + '\n')
            logger.info('Tag added to output: %s' % tag.name)
