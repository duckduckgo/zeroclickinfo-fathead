#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A Fathead parser for wikiHow articles.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data should exist in the data/html directory. If it doesn't, make
# sure that the ./fetch.sh script has been run to scrape the html data
# from the wikiHow website. You will need to run this script from the
# `lib/fathead/wikihow` subdirectory
#
# Please read wikihow's service agreement before running this program
# http://www.wikihow.com/wikiHow:Terms-of-Use

__MODULE__ = "wikihow"
__AUTHOR__ = "DuckDuckGo [https://duckduckgo.com]"
__SOURCE__ = "http://www.wikihow.com/"
__OUTPUT__ = "output.txt"

import argparse
import glob
from multiprocessing import cpu_count, Pool
import os
import re

from bs4 import BeautifulSoup

CPU_COUNT = cpu_count()

OUTPUT_TEMPLATE = """\
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
        with open(filename, 'r', encoding="utf-8") as f:
            contents = f.read()
            parsed_contents = parse_html(contents)
            formatted_output = format_output(parsed_contents)
            write_to_output(formatted_output)
    except:
        pass # parser can't parse

def format_output(doc):

    # formats the intro
    intro = FATHEAD_TEMPLATE.format(
                intro=doc["intro"],
                list_items=doc["points"]
            )
    intro = "".join(intro.splitlines())

    # fh expectation for image format
    if not doc["image"] == "":
        doc["image"] = "[[Image:" + val["image"] + "]]"

    document = OUTPUT_TEMPLATE.format(
        title = doc["title"],
        entry_type = "A",
        redirect_title = "",
        categories = "",
        empty_field = "",
        related_topics = "",
        external_links = "",
        disambiguation = "",
        image = doc["image"],
        abstract = intro,
        url = doc["url"],
    )
    return document

def write_to_output(article):
    """
    Appends the parsed article to the `output.txt` file.
    """
    with open('output.txt', 'a', encoding="utf-8") as f:
        f.write(article + "\n")

def parse_html(doc):
    soup = BeautifulSoup(doc, "html.parser")
    parsed_doc = {} # we'll build a dictionary of the article and pass it back

    ## Gets the title
    [x.extract() for x in soup.h1.findAll("span")]
    title = soup.h1.text
    parsed_doc["title"] = title
    print("Parsing:", title)

    # Gets the article image if it exists
    if soup.find("meta", property="og:image"):
        image = soup.find("meta", property="og:image")["content"]
    else:
        image = ""
    parsed_doc["image"] = image

    ## Gets the URIs
    url = soup.find("link", rel="canonical")["href"]
    parsed_doc["url"] = url

    ## Gets if expert reviewed
    if soup.find("a", {"class" : "sp_intro_expert"}):
        expert_reviewed = True
    else:
        expert_reviewed = False
    parsed_doc["expert"] = expert_reviewed

    ## Gets the intro text
    intro = soup.find(id="intro").find_all("p")[-1].text
    intro = re.sub(re.compile("\[\d+\]"), "", intro) # removes reference
    parsed_doc["intro"] = intro

    ## Gets the high level points
    points = soup.find_all("b", {"class" : "whb"})
    if len(points) < 4:
        parsed_doc["points"] = ""
    else:
        points_list = list()
        points = [x.text for x in points]
        points = ["<li> - " + x + "</li>" for x in points]
        points = "".join(points)
        parsed_doc["points"] = points
    return parsed_doc

if __name__ == "__main__":

    argparser = argparse.ArgumentParser()
    argparser.add_argument("data_folder",
                        help="The folder containing the data")
    argparser.add_argument("-p", "--processes",
                        help="Number of parrallel processes to parse corpus",
                        type=int,
                        default=CPU_COUNT)

    args = argparser.parse_args()
    folder = args.data_folder
    files_to_parse = glob.glob(os.path.join(folder, "*.txt"))

    pool = Pool(args.processes)
    pool.map(parse_file, files_to_parse)

