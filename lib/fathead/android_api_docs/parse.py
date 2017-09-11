#!/usr/bin/env

import re
from bs4 import BeautifulSoup
from os import walk
from collections import defaultdict


BASE_URL = 'https://developer.android.com/'
ROOT_DIRECTORY = './download/reference'
OUTPUT_FILE = 'output.txt'


def remove_anchor_tags(method_details):
    for a in method_details.findAll('a'):
        a.unwrap()


def build_data(title, type, disambiguation='', abstract='', url=''):
    return [
        title,  # title
        type,  # type is article
        '',  # no redirect data
        '',  # ignore
        '',  # no categories
        '',  # ignore
        '',  # no related topics
        '',  # ignore
        '',  # external link
        disambiguation,  # no disambiguation
        '',  # images
        abstract,  # abstract
        url,  # anchor to specific section
    ]


def write_data_to_file(f, data):
    data = '\t'.join(data)
    f.write('{}\n'.format(data))


def gather_articles():
    """
    Walks over all of the HTML downloaded by fetch.py and stores information from each article into a dictionary
    :return: A Dictionary Mapping Article titles to a list of all entries with that title.
    """
    articles = defaultdict(list)

    for dir_name, subdir_list, file_list in walk(ROOT_DIRECTORY):

        # These are all .html files in a package, one of which is package-summary.html
        for fname in file_list:
            file_path = dir_name + "/" + fname
            soup = BeautifulSoup(open(file_path), 'html.parser')
            # The package summary needs to be built differently
            if fname == "package-summary.html":
                parse_summary_page(articles, file_path, fname, soup)
            else:
                # Build regular article with code highlighting
                parse_class_documentation(articles, file_path, fname, soup)
    return articles


def parse_summary_page(articles, file_path, fname, soup):
    print("\tThe summary file:" + fname)
    data = build_summary_article(soup, file_path)
    if data is not None:
        articles[data[0]].append(data)
    parse_table_entries(soup, articles, file_path)


def parse_table_entries(soup, articles, file_path):
    """
    Adds articles for the entries in the table that don't have a hyperlink to class documentation.
    """
    for tr in soup.findAll('tr'):
        if len(tr.select('td.jd-linkcol a')) == 0 and len(tr.select('td.jd-linkcol')) == 1:
            title = tr.select('td.jd-linkcol')[0].text
            description = ' '.join(tr.select('td.jd-descrcol')[0].decode_contents(formatter="html").split())
            description = description.replace('&nbsp;', '')
            if description != '':
                data = build_data(title, 'A', abstract=description, url=BASE_URL + file_path[11:])
                articles[data[0]].append(data)


def build_summary_article(soup, file_path):
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

    return build_data(title, 'A', abstract=abstract, url=url)


def parse_class_documentation(articles, file_path, fname, soup):
    print('\tThe file:%s' % fname)
    data = build_article(soup, file_path, fname)
    if data is not None:
        articles[data[0]].append(data)


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

    # HTML is not formed nicely enough to use bs4 to parse, so default to regex.
    match = re.match(r'^\s*<p>(.*?)</?p>', class_description, re.DOTALL)
    if match is not None:
        class_description = ' '.join(match.group(1).split())
    else:
        class_description = ''

    # Build the abstract from the description and example code usage
    abstract = "<pre><code>{}</code></pre>".format(class_signature)
    abstract += class_description
    abstract = '<section class="prog__container">{}</section>'.format(abstract)

    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    print("=======================================================================")

    return build_data(title, 'A', abstract=abstract, url=url)


def adjust_titles(articles):
    index = -2
    titles_same = True
    while titles_same:
        titles = [article[0] for article in articles]
        same_count = 0
        for article in articles:
            if titles.count(article[0]) > 1:
                same_count += 1
                # Include the next level package to try and get a unique title.
                article[0] = article[12][:-5].split('/')[index] + '.' + article[0]
        if same_count == 0:
            titles_same = False
        else:
            index -= 1


def write_articles_to_output(articles, file_name):
    """
    Given a dictionary mapping titles to all articles associated with that title, finds the shortest unique title for
      each of the entries and adds a disambiguation entry if necessary.
    """
    with open(file_name, 'w') as fp:
        for article in articles.values():
            if len(article) == 1:
                write_data_to_file(fp, article[0])
            else:  # Multiple articles with the same title.
                common_title = article[0][0]
                disambiguation_string = '*'

                adjust_titles(article)

                for entry_article in article:  # Add article addressed by unique classpath
                    write_data_to_file(fp, entry_article)
                    disambiguation_string += '[[{}]], {}\\n*'.format(entry_article[0], entry_article[11])

                disambiguation_string = disambiguation_string[:-3]
                data = build_data(common_title, 'D', disambiguation=disambiguation_string)
                write_data_to_file(fp, data)


def main():
    articles = gather_articles()
    write_articles_to_output(articles, OUTPUT_FILE)


if __name__ == '__main__':
    main()
