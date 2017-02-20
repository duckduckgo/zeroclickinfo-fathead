#!/usr/bin/env python3.5
# -*- coding: utf-8 -*-

"""Parse Pytest docs"""

from bs4 import BeautifulSoup
import re

__MODULE__ = 'PYTEST FATHEAD'
__AUTHOR__ = 'adityatandon007@DDG_COMMUNITY'

INFILE = 'download/docs.html'
OUTFILE = 'output.txt'

abstract_fmt = '<section class="prog__container"><p>{}{}</p></section>'
url_fmt = 'http://doc.pytest.org/en/latest/builtin.html%s'


def clean_output(output):
    """Remove newlines from abstract."""
    return re.sub('\r?\n+', ' ', output)


def clean_code(code):
    """Escape newlines."""
    return code.replace('\n', '\\n')


def generate_api_example(api_examples):
    """Generate a new output entry."""
    for api_example in api_examples:
#       print(api_example)
        title = api_example.dt.get('id')
        link = api_example.find("a", {'class': 'headerlink'}).extract()
        url = url_fmt % link['href']

        # if the example contains code, add it to the abstract
        if api_example.pre:
            code = clean_code(api_example.pre.text)
            code = '<pre><code>{}</code></pre>'.format(code)
        else:
            code = ''

        abstract = clean_output(api_example.dd.p.text)
        abstract = abstract_fmt.format(abstract, code)

        yield [title, 'A', '', '', '', '', '', '', '', '', '', abstract, url]


def main():
    with open(INFILE, 'r', encoding='utf-8') as f, open(OUTFILE, 'w', encoding='utf-8') as o:

        html = BeautifulSoup(f, 'html.parser')
        api_examples = html.select('dl[class=function]')

        for api_example in generate_api_example(api_examples):
            output = '\t'.join(api_example)
            o.write(output + '\n')


if __name__ == '__main__':
    print('STARTING ' + __MODULE__ + ' PARSING')
    print('PARSING ...')
    main()
    print('ENDING ' + __MODULE__ + ' PARSING')