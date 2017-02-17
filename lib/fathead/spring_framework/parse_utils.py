#!/usr/bin/python
from bs4 import BeautifulSoup
import os
import re

BASE_JAVADOC_URL = "http://docs.spring.io/spring/docs/current/javadoc-api/"
BASE_LOCAL_JAVADOC_DIR = "./docs/javadoc-api"
BASE_JAVADOC_FILE = BASE_LOCAL_JAVADOC_DIR + "/allclasses-noframe.html"


def read_root_file():
    rootFile = open(BASE_JAVADOC_FILE, 'r')
    lines = rootFile.read()
    rootFile.close()
    return lines


def collect_doc_files_from(dir):
    docFiles = []
    for (path, dirs, files) in os.walk(dir):
        if 'class-use' not in path and 'index-files' not in path:
            for f in files:
                if f.endswith('.html') and 'package-' not in f and 'doc-files' not in f:
                    docFiles.append("%s/%s" % (path, f))
    return docFiles


def get_docs(filename, classUrl):
    if filename.endswith('.html') and 'package-' not in filename and 'doc-files' not in filename:
        content = BeautifulSoup(get_content(filename), 'html.parser')
        classname = remove_keywords(content.find_all('h2')[0].string)
        block = content.find_all('div', 'block', limit=1)
        description = ""
        if len(block) > 0:
            description = block[0].get_text()
            description = cut_length(description)
        url = ""
        if len(classUrl) != 0:
            url = BASE_JAVADOC_URL + classUrl
        return classname, description, url


def cut_length(description):
    #  if len(description) > 100:
    description = description[0:description.rfind('.', 0, 300) + 1]
    return description.replace("\n", "")


def remove_keywords(line):
    if isinstance(line, basestring):
        line = re.sub(r'<\w,?\w?>', '', line)
        return line.replace('Class ', '').replace('Enum ', '').replace('Interface ', '').replace('Annotation Type ', '')
    else:
        return ''


def get_content(filename):
    f = open(filename, 'r')
    lines = f.read()
    f.close()
    return lines


def concat_list(data_list=['', '', '']):
    if data_list != None:
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


def concat(title, entry_type, abstract='', url='', redirect_location=''):
    four = ''
    categories = ''
    six = ''
    related_topics = ''  # [[Perl Data Language|PDL]], can be multiples?
    eight = ''
    external_links = ''  # [$url title text]\\n, can be multiples
    ten = ''
    image = ''

    data = [title, entry_type, redirect_location, four, categories, six,
            related_topics, eight, external_links, ten, image, abstract, url]
    line = "\t".join(data) + "\n"
    return line


def add_redirects(f, clazz):
    # Regex splits on uppercase letters.  Won't split on SQL since it's used in a few class names
    uppercase_words = re.findall(r'[A-Z](?:QL)?[^A-Z]*', clazz)
    if len(uppercase_words) > 1:
        redirect_title = ' '.join(uppercase_words)
        line = concat_redirect(redirect_title, clazz)
        f.write(line.encode('utf'))


def output(filename, data_list):
    line = concat_list(data_list)
    if not line.startswith("No class found") and line != "" and not ("No abstract found" in line):
        f = open(filename, 'a')
        f.write(line.encode('utf'))
        add_redirects(f, data_list[0])
        f.close()
