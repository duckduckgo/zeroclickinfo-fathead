#!/usr/bin/env

import re
from bs4 import BeautifulSoup
from os import walk


BASE_URL = 'https://developer.android.com/'


def remove_anchor_tags(method_details):
    for a in method_details.findAll('a'):
        a.unwrap()


def build_summary_article(soup, file_path, file_name):
    title_list = soup.select('#jd-content > h1')
    if len(title_list) == 0:
        raise Exception('Summary title not found for file {}'.format(file_path))
    title = title_list[0].text

    file_path = file_path[11:]
    url = BASE_URL + file_path

    abstract_paragraphs = soup.select('#jd-content > p')

    if len(abstract_paragraphs) > 0:  # For brevity's sake, just take the first paragraph of the package's summary
        abstract = abstract_paragraphs[0].text
    else:  # There's no abstract for us to provide. So don't include this article
        return None

    # Remove carriage returns and extra whitespace
    abstract = re.sub(r'\s+', ' ', abstract)

    # Use DuckDuckHack's recommended code snippet wraps
    abstract = abstract.replace("<code>", "<pre><code>")
    abstract = abstract.replace("</code>", "</code></pre>")

    abstract = '<section class="prog__container">{}</section>'.format(abstract)

    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    print("=======================================================================")

    return [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            url,             # anchor to specific section
        ]


def build_article(soup, file_path, file_name):
    title = file_name.partition(".html")[0]

    file_path = file_path[11:]
    url = BASE_URL + file_path

    class_signature_elements = soup.select('#jd-content > p > code.api-signature')

    class_signature = ''
    for signature_element in class_signature_elements:
        remove_anchor_tags(signature_element)
        class_signature += ' '.join(signature_element.decode_contents(formatter="html").split())
        class_signature += ' '
    class_signature = class_signature[:-1]

    class_description = soup.findAll('hr')[0]
    remove_anchor_tags(class_description)
    class_description = class_description.decode_contents(formatter="html")
    match = re.match(r'^\s*<p>(.*?)</?p>', class_description, re.DOTALL)
    if match is not None:
        class_description = ' '.join(match.group(1).split())
    else:
        class_description = ''

    # Use DuckDuckHack's recommended code snippet wraps
    class_description = class_description.replace("<code>", "<pre><code>")
    class_description = class_description.replace("</code>", "</code></pre>")

    # Build the abstract from the description and example code usage
    abstract = class_description
    abstract += "<pre><code>{}</code></pre>".format(class_signature)
    abstract = '<section class="prog__container">{}</section>'.format(abstract)

    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    print("=======================================================================")

    return [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            url,             # anchor to specific section
        ]

if __name__ == '__main__':
    with open('output.txt', 'w') as fp:
        # Set the directory you want to start from
        rootDir = './download/reference'
        for dirName, subdirList, fileList in walk(rootDir):

            # These are all .html files in a package, one of which is package-summary.html
            for fname in fileList:
                filePath = dirName + "/" + fname
                soup = BeautifulSoup(open(filePath), 'html.parser')
                # The package summary needs to be built differently
                if fname == "package-summary.html":
                    print("\tThe summary file:" + fname)
                    data = build_summary_article(soup, filePath, fname)
                else:
                    # Build regular article with code highlighting
                    print('\tThe file:%s' % fname)
                    data = build_article(soup, filePath, fname)

                if data is not None:
                    data = '\t'.join(data)
                    fp.write('{}\n'.format(data))
