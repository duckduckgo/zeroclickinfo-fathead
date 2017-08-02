#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parser for Apple Discussion articles.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#

__MODULE__ = "apple_discussions"
__AUTHOR__ = "DuckDuckGo [https://duckduckgo.com]"
__SOURCE__ = "https://discussions.apple.com/"
__CONTACT__ = "pjhampton[at]protonmail.com"
__OUTPUT__ = "output.txt"

import argparse
import glob
from multiprocessing import cpu_count, Pool
import os
import re

from bs4 import BeautifulSoup

CPU_COUNT = cpu_count()
not_parsed = list()

OUTPUT = """\
{title}\t
{entry_type}\t
{redirect_title}\t
{empty_field}\t
{categories}\t
{empty_field}\t
{related_topics}\t
{empty_field}\t
{external_links}\t
{disambiguation}\t
{image}\t
{abstract}\t
{url}
""".replace("\n", "")

FATHEAD_TEMPLATE = """\
<section class="prog__container">
<p>{intro}</p>
<span class="prog__sub">
<ol>{list_items}</ol>
</section>
"""

def parse_file(filename):
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            contents = f.read()
            parsed_document = parse_html(contents, filename)
            if parsed_document != False:
                output = format_output(parsed_document)
                write_to_output(output)
    except:
        pass # parser can't parse

def parse_html(doc, url):
    soup = BeautifulSoup(doc, "html.parser")
    parsed_doc = {}
    pop_url_extension = url.split(".")[-2]
    url_id = pop_url_extension.split("/")[-1]
    parsed_doc["url"] = "https://discussions.apple.com/thread/" + url_id

    title = soup.h1.text
    parsed_doc["title"] = title.replace("Q:", "").strip()

    if soup.find("div", {"class": "recommended-answers"}):
        parsed_doc["body"] = soup.find("div", {"class", "recommended-answers"}).find("section").find("div", {"class": "jive-rendered-content"}).text
    else:
        return False

    return parsed_doc


def format_output(doc):

    document = OUTPUT.format(
        title = doc["title"],
        entry_type = "A",
        redirect_title = "",
        categories = "",
        empty_field = "",
        related_topics = "",
        external_links = "",
        disambiguation = "",
        image = "",
        abstract = doc["body"],
        url = doc["url"],
    )

    return document

def write_to_output(article):
    """
    Appends the parsed article to the `output.txt` file.
    """
    with open(__OUTPUT__, 'a', encoding="utf-8") as f:
        f.write(article + "\n")

if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("data_folder",
        help="The folder containing the data")
    argparser.add_argument("-p", "--processes",
        help="Number of parrallel processes to parse corpus", type=int, default=CPU_COUNT)

    args = argparser.parse_args()
    folder = args.data_folder
    files_to_parse = glob.glob(os.path.join(folder, "*.txt"))

    for filename in files_to_parse:
        parse_file(filename)
