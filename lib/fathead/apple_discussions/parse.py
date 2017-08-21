#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parser for Apple Discussion articles.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# For further information see https://duck.co/ia/view/apple_discussions

__MODULE__ = "apple_discussions"
__AUTHOR__ = "DuckDuckGo [https://duckduckgo.com]"
__SOURCE__ = "https://discussions.apple.com/"
__OUTPUT__ = "output.txt"
__DEBUG__ = False

import argparse
import glob
from multiprocessing import cpu_count, Pool
import os
import re

from bs4 import BeautifulSoup

CPU_COUNT = cpu_count()

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

FHTEMPLATE = """\
<p><b>Answered by {username} ({date})</b></p>
{information}
""".replace("\n", "")

def parse_file(filename):
    """
    The pipeline to process the files
    """
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            contents = f.read()
            parsed_document = parse_html(contents, filename)
            if parsed_document != False:
                output = format_output(parsed_document)
                write_to_output(output)
            # If we can't parsed and __DEBUG__ is switched on we log to "errors.txt"
            elif __DEBUG__ == True and parsed_document == False:
                write_to_output(filename, output="errors.txt")
    except:
        pass # parser can't parse

def parse_html(doc, url):
    """
    Parses the html docs for a Title, Answer and URL
    """
    soup = BeautifulSoup(doc, "html.parser")
    parsed_doc = {}

    # Builds the original URL
    pop_url_extension = url.split(".")[-2]
    url_id = pop_url_extension.split("/")[-1]
    parsed_doc["url"] = "https://discussions.apple.com/thread/" + url_id

    # Gets the title
    title = soup.h1.text
    temp_title = title.strip().replace("Q: ", "")
    parsed_doc["title"] = temp_title[0].capitalize() + temp_title[1:] # Capitalizes first word
    print("Parsing", parsed_doc["title"])

    # Adds category to the title (if we can)
    try:
        topic = soup.find("a", {"class": "jive-breadcrumb-last"}).text
        topic = topic.replace("Using ", "")
        parsed_doc["title"] += " ({topic})".format(topic=topic)
    except:
        pass

    # Get's the most 'Recommended Answer'
    if soup.find("div", {"class": "recommended-answers"}):

        for p in soup.findAll('p'):
            if p.string:
                p.string.replace_with(p.string.strip())

        # ditch the span tags
        for span in soup.findAll('span'):
            span.decompose()

        username = soup.find("div", {"class", "recommended-answers"}).find("a", {"class": "username"}).text
        username = username.strip()

        posted = soup.find("div", {"class", "recommended-answers"}).find("p", {"class": "meta-posted"}).text
        posted = posted.strip().replace("Posted on ", "")
        posted = posted.split(" ")
        posted = posted[0] + " " + posted[1] + " " + posted[2]

        content = soup.find("div", {"class", "recommended-answers"}).find("section").find("div", {"class": "jive-rendered-content"})

        for tags in content.findAll(True):
            tags.attrs = {}

        # Does some regex replacements that the parser just won't hit with grace
        contents = FHTEMPLATE.format(information=str(content), username=username, date=posted)
        contents = re.sub(re.compile("<p></p>"), "", contents)
        contents = re.sub(re.compile("<div ((.|\n)+?)>"), "", contents)
        contents = re.sub(re.compile("</div>"), "", contents)
        contents = re.sub(re.compile("<a>"), "", contents)
        contents = re.sub(re.compile("</a>"), "", contents)
        contents = re.sub(re.compile("\n"), "\\n", contents)
        parsed_doc["body"] = contents

        # Some last moment validation
        if "CodeBlockStart" in parsed_doc["body"]: # contains code
            return False
        if "blockquote" in parsed_doc["body"]:
            return False
        if len(parsed_doc["body"].split(" ")) > 500:
            return False
        if len(parsed_doc["body"].split(" ")) < 30:
            return False
        if parsed_doc["url"] == "" or parsed_doc["url"] == None:
            return False
    else:
        return False

    return parsed_doc


def format_output(doc):
    """
    Takes dict doc and formats it for the final output
    """
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

def write_to_output(article, output=__OUTPUT__):
    """
    Appends the parsed article to the `output.txt` file.
    """
    with open(output, 'a', encoding="utf-8") as f:
        f.write(article + "\n")

if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("data_folder",
        help="The folder containing the data. Maybe run parse.sh instead?")
    argparser.add_argument("-p", "--processes",
        help="Number of parrallel processes to parse corpus", type=int, default=CPU_COUNT)

    args = argparser.parse_args()
    folder = args.data_folder
    files_to_parse = glob.glob(os.path.join(folder, "*.txt"))

    pool = Pool(args.processes)
    pool.map(parse_file, files_to_parse)

