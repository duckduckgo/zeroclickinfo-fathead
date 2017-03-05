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
    if len(title_list) != 1:
        raise Exception('Summary title not found for file {}'.format(file_name))
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
    title = str(file_name).partition(".html")[0]

    file_path = file_path[11:]
    url = BASE_URL + file_path

    # On the off chance soup can't find the desired snippets, use fallback
    # First line of expample code listed
    first_code = soup.find('code')
    remove_anchor_tags(first_code)
    # Second line of example code in doc
    second_code = first_code.find_next_sibling().find('code')
    remove_anchor_tags(second_code)

    # Credit for crlf removal: http://stackoverflow.com/questions/16149649/remove-carriage-return-in-python
    # Credit for the sporadic whitespace removal: http://stackoverflow.com/questions/2077897/substitute-multiple-whitespace-with-single-whitespace-in-python
    first_code = first_code.contents[0].encode('utf-8')
    first_code = ''.join(first_code.splitlines())
    first_code = ' '.join(first_code.split())

    # Credit for full inner html: http://stackoverflow.com/questions/8112922/beautifulsoup-innerhtml
    second_code = second_code.decode_contents(formatter="html")
    second_code = second_code.encode('utf-8')
    # Remove carriage returns and extra whitespace
    second_code = ''.join(second_code.splitlines())
    second_code = ' '.join(second_code.split())

    class_description = soup.find("hr").find_next("p")
    remove_anchor_tags(class_description)
    class_description = class_description.prettify()
    class_description = class_description.encode('utf-8')

    # Google unfortunately didn't close quite a few p tags, so we need to grab the description contents this way
    # Credit for partition: http://stackoverflow.com/questions/14801057/python-splitting-to-the-newline-character
    class_description = class_description.partition("<h2")[0]

    class_description = class_description.partition("<h3")[0]

    class_description = class_description.partition("<div")[0]

    # Add missing closing p tags where needed
    class_description = class_description.partition("</p>")[0]
    class_description += "</p>"

    # Remove carriage returns and extra whitespace
    class_description = ''.join(class_description.splitlines())
    class_description = ' '.join(class_description.split())

    # Use DuckDuckHack's recommended code snippet wraps
    class_description = class_description.replace("<code>", "<pre><code>")
    class_description = class_description.replace("</code>", "</code></pre>")

    # Build the abstract from the description and example code usage
    abstract = ""
    if re.search(r'<p>\s*</p>', class_description) is None:
        abstract += class_description.encode('utf-8')
    abstract += "<pre><code>%s</code></pre>" % ((first_code + "\\n" + second_code).encode('utf-8'))
    abstract = '<section class="prog__container">%s</section>' % abstract

    abstract = abstract.encode('utf-8')

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
                    if data is not None:
                        data = '\t'.join(data)
                        fp.write('{}\n'.format(data))
                else:
                    # Build regular article with code highlighting
                    print('\tThe file:%s' % fname)
                    # data = build_article(soup, filePath, fname)
                    # data = '\t'.join(data)
                    # fp.write('{}\n'.format(data))
