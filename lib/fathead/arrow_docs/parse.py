"""Parse Arrow docs."""

from bs4 import BeautifulSoup
import codecs
import re

INFILE = 'download/docs.html'
OUTFILE = 'output.txt'

abstract_fmt = '<section class="prog__container"><p>{}</p>{}</section>'
url_fmt = 'https://arrow.readthedocs.io/en/latest/%s'


def clean_output(output):
    """Remove newlines from abstract."""
    return re.sub('\r?\n+', '', output)


def clean_code(code):
    """Escape newlines."""
    return code.replace('\n', '\\n')


def yield_api_example(api_examples):
    """Yields a new output entry."""
    for api_example in api_examples:

        title = api_example.dt.get('id')
        url = url_fmt % api_example.a.get('href')

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
    with codecs.open(INFILE, 'rb', encoding='utf-8') as f, \
        codecs.open(OUTFILE, 'wb', encoding='utf-8') as o:

        html = BeautifulSoup(f, 'html.parser')
        api_examples = html.select('dl[class=method]')

        for api_example in yield_api_example(api_examples):
            output = '\t'.join(api_example)
            o.write(output + '\n')


if __name__ == '__main__':
    main()
