#!/usr/bin/env

import re
from bs4 import BeautifulSoup
from os import walk
from collections import defaultdict


BASE_URL = 'https://developer.android.com/'
OUTPUT_FILE = 'output.txt'


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

    # Build the abstract from the description and example code usage
    abstract = "<pre><code>{}</code></pre>".format(class_signature)
    abstract += class_description
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


def titles_same(articles, index):
    titles = [article[0] for article in articles]
    same_count = 0
    for article in articles:
        if titles.count(article[0]) > 1:
            same_count += 1
            article[0] = article[12][:-5].split('/')[index] + '.' + article[0]
    return same_count > 0


def parse_table_entries(soup, articles, file_path):
    for tr in soup.findAll('tr'):
        if len(tr.select('td.jd-linkcol a')) == 0 and len(tr.select('td.jd-linkcol')) == 1:
            title = tr.select('td.jd-linkcol')[0].text
            description = ' '.join(tr.select('td.jd-descrcol')[0].decode_contents(formatter="html").split())
            description = description.replace('&nbsp;', '')
            if description != '':
                data = [
                    title,  # title
                    'A',  # type is article
                    '',  # no redirect data
                    '',  # ignore
                    '',  # no categories
                    '',  # ignore
                    '',  # no related topics
                    '',  # ignore
                    '',  # external link
                    '',  # no disambiguation
                    '',  # images
                    description,  # abstract
                    BASE_URL + file_path[11:],  # anchor to specific section
                ]
                articles[data[0]].append(data)


def write_data_to_file(f, data):
    data = '\t'.join(data)
    fp.write('{}\n'.format(data))


def gather_articles():
    articles = defaultdict(list)

    # Set the directory you want to start from
    root_dir = './download/reference'
    for dir_name, subdir_list, file_list in walk(root_dir):

        # These are all .html files in a package, one of which is package-summary.html
        for fname in file_list:
            file_path = dir_name + "/" + fname
            soup = BeautifulSoup(open(file_path), 'html.parser')
            # The package summary needs to be built differently
            if fname == "package-summary.html":
                print("\tThe summary file:" + fname)
                data = build_summary_article(soup, filePath, fname)
                if data is not None:
                    articles[data[0]].append(data)
                parse_table_entries(soup, articles, file_path)
            else:
                # Build regular article with code highlighting
                print('\tThe file:%s' % fname)
                data = build_article(soup, filePath, fname)
                if data is not None:
                    articles[data[0]].append(data)
    return articles


def write_articles_to_output(articles, file_name):
    with open(file_name, 'w') as fp:
        for article in articles.values():
            if len(article) == 1:
                write_data_to_file(fp, article[0])
            else:
                common_title = article[0][0]
                disambiguation_string = '*'

                split_index = -2
                while titles_same(article, split_index):
                    split_index -= 1

                for entry_article in article:  # Add article addressed by unique classpath
                    write_data_to_file(fp, entry_article)
                    disambiguation_string += '[[{}]], {}\\n*'.format(entry_article[0], entry_article[11])
                disambiguation_string = disambiguation_string[:-3]
                data = [
                    common_title,  # title
                    'D',  # type is article
                    '',  # no redirect data
                    '',  # ignore
                    '',  # no categories
                    '',  # ignore
                    '',  # no related topics
                    '',  # ignore
                    '',  # external link
                    disambiguation_string,  # disambiguation
                    '',  # images
                    '',  # abstract
                    '',  # anchor to specific section
                ]
                write_data_to_file(fp, data)


def main():
    articles = gather_articles()
    write_articles_to_output(articles, OUTPUT_FILE)


if __name__ == '__main__':
    main()


