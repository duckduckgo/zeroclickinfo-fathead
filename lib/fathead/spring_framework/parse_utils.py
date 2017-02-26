#!/usr/bin/python
from bs4 import BeautifulSoup
import os
import re

BASE_JAVADOC_URL = "http://docs.spring.io/spring/docs/current/javadoc-api/"
BASE_LOCAL_JAVADOC_DIR = "./docs/javadoc-api"
BASE_JAVADOC_FILE = BASE_LOCAL_JAVADOC_DIR + "/allclasses-noframe.html"


def read_root_file():
    root_file = open(BASE_JAVADOC_FILE, 'r')
    lines = root_file.read()
    root_file.close()
    return lines


def collect_doc_files_from(dir):
    doc_files = []
    for (path, dirs, files) in os.walk(dir):
        if 'class-use' not in path and 'index-files' not in path:
            for f in files:
                if f.endswith('.html') and 'package-' not in f and 'doc-files' not in f:
                    doc_files.append("%s/%s" % (path, f))
    return doc_files


def get_docs(filename, class_path):
    if filename.endswith('.html') and 'package-' not in filename and 'doc-files' not in filename:
        content = BeautifulSoup(get_content(filename), 'html.parser')
        classname = remove_keywords(content.find_all('h2')[0].text)
        block = content.find_all('div', 'block', limit=1)
        description = ""
        if len(block) > 0:
            description = block[0].get_text()
            description = cut_length(description)
        url = ""
        if len(class_path) != 0:
            url = '{}{}.html'.format(BASE_JAVADOC_URL, class_path)
        return classname, description, url, class_path.replace('/', '.')


def cut_length(description):
    #  if len(description) > 100:
    description = description[0:description.rfind('.', 0, 300) + 1]
    return description.replace("\n", "")


def remove_keywords(line):
    if isinstance(line, str):
        line = re.sub(r'<.*?>', '', line)
        return line.replace('Class ', '').replace('Enum ', '').replace('Interface ', '').replace('Annotation Type ', '')
    else:
        return ''


def get_content(filename):
    f = open(filename, 'r')
    lines = f.read()
    f.close()
    return lines


def concat_list(data_list=['', '', '']):
    if data_list is not None:
        return concat_article(data_list[0], data_list[1], data_list[2])
    else:
        return ""


def concat_article(clazz, description, url):
    description = description.replace("\n", "\\n").replace("\t", "\\t") or "No abstract found"
    abstract = '<section class="prog__container">' + description + '</section>'

    url = url or "No URL found"

    return concat(clazz, 'A', abstract=abstract, url=url)


def concat_redirect(title, redirect_location):
    return concat(title, 'R', redirect_location=redirect_location)


def concat(title, entry_type, abstract='', url='', redirect_location='', disambiguaions=''):
    four = ''
    categories = ''
    six = ''
    related_topics = ''  # [[Perl Data Language|PDL]], can be multiples?
    eight = ''
    external_links = ''  # [$url title text]\\n, can be multiples
    image = ''

    data = [title, entry_type, redirect_location, four, categories, six,
            related_topics, eight, external_links, disambiguaions, image, abstract, url]
    line = "\t".join(data) + "\n"
    return line


def add_redirects(f, clazz):
    # We eliminate the outer class for inner class entries
    separator = clazz.find('.')
    if separator != -1:
        line = concat_redirect(clazz[separator + 1:], clazz)
        f.write(line)
        clazz = clazz[separator + 1:]

    # Regex splits on uppercase letters.  Won't split on SQL since it's used in a few class names
    uppercase_words = re.findall(r'[A-Z](?:QL)?[^A-Z]*', clazz)
    if len(uppercase_words) > 1:
        redirect_title = ' '.join(uppercase_words)
        line = concat_redirect(redirect_title, clazz)
        f.write(line)


def add_article(f, article_data, should_redirect=True):
    line = concat_list(article_data)
    if line != "" and not ("No abstract found" in line):
        f.write(line)
        if should_redirect:
                add_redirects(f, article_data[0])


def add_disambiguation(f, title, linked_entries):
    disambiguation_string = '*'
    for entry in linked_entries:
        disambiguation_string += '[[{}]], {}\\n*'.format(entry[3], entry[1])
    disambiguation_string = disambiguation_string[:-3]
    line = concat(title, 'D', disambiguaions=disambiguation_string)
    f.write(line)


def output(filename, data_list):
    f = open(filename, 'a')
    if len(data_list) == 1:  # There is only one class with the given name
        add_article(f, data_list[0])
    else:  # There are multiple articles sharing the same title.  So we need a disambiguation entry.
        common_title = data_list[0][0]
        for article in data_list:
            add_article(f, (article[3], article[1], article[2]), False)
        add_redirects(f, common_title)
        add_disambiguation(f, common_title, data_list)
    f.close()
