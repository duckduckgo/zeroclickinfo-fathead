import re
import codecs

from bs4 import BeautifulSoup
from os.path import relpath, join, dirname

url = "https://en.wikipedia.org/wiki/List_of_HTTP_status_codes"


def status_codes():
    """ Get and format the status code data.

    Parse the wikipedia data file, and generate a dictionary, which keys
    are the name of each status code, and value the description of given
    code.

    """
    data_filepath = relpath(join(dirname(__file__), 'download', 'data'))
    soup = BeautifulSoup(open(data_filepath).read())
    codes = {}
    status_codes_dl = soup.select('dl')
    for dl in status_codes_dl:
        for title, details in zip(dl.find_all('dt'), dl.find_all('dd')):
            codes[title.text] = re.sub(r'\[\d+\]', '', details.text)
    return codes


# Generate the output.txt file
with codecs.open('output.txt', 'w', 'utf-8') as out:
    codes = status_codes()
    for title, c in sorted(codes.items()):
        out.write('\t'.join([
            '%s' % title,         # title
            'A',                  # type
            '',                   # redirect
            '',                   # other uses
            '',                   # categories
            '',                   # reference
            '',                   # see also
            '',                   # further reading
            '%s' % (url),         # external links
            '',                   # disambiguation
            '',                   # images
            '%s' % codes[title],  # abstract
            '',                   # source url
            '\n'
        ]))
