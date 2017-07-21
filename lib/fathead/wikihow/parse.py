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

import os
import re

from bs4 import BeautifulSoup

cache = list() # holds parsed documents (dists)
output = list() # holds parsed documents (strings)
errors = list() # holds parsing errors (dicts)

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

FHTEMPLATE = """\
<section class="prog__container">
<p>{intro}</p>
<span class="prog__sub">
<ol>{list_items}</ol>
</section>
"""

class Parser(object):
    # Loops through al the files in the /data/html/ directory
    def run():
        for path, _subdirs, files in os.walk('data/html'):
            for filename in files:
                try:
                    with open(path + "/" + filename, 'r', encoding="utf-8") as f:
                        contents = f.read()
                        parse_html(contents)
                except KeyboardInterrupt:
                    # so the user can kill the program
                    break
                except:
                    pass
    print("No more articles to parse.")

    def write_output_document(output):
        output = list(set(output)) # kill off duplicates
        with open('output.txt', 'w', encoding="utf-8") as f:
            for article in output:
                f.write(article + "\n")
        print("Output.txt file created")

    # creates formats the article for output.txt (does not write)
    def create_output(output_dict):
        for key, val in enumerate(output_dict):

            # formats the intro
            intro = FHTEMPLATE.format(
                        intro=val["intro"],
                        list_items=val["points"]
                    )
            intro = "".join(intro.splitlines())

            # fh expectation for image format
            if not val["image"] == "":
                val["image"] = "[[Image:" + val["image"] + "]]"

            document = OUTPUT_TEMPLATE.format(
                title = val["title"],
                entry_type = "A",
                redirect_title = "",
                categories = "",
                empty_field = "",
                related_topics = "",
                external_links = "",
                disambiguation = "",
                image = val["image"],
                abstract = intro,
                url = val["url"],
            )

            output.append(document);

def parse_html(doc):
    soup = BeautifulSoup(doc, "html.parser", from_encoding="utf-8")
    tmpdoc = {} # we'll build a dictionary of the article and pass it back

    ## Gets the title
    [x.extract() for x in soup.h1.findAll("span")]
    title = soup.h1.text
    tmpdoc["title"] = title
    print("title:", title)

    # Gets the article image if it exists
    if soup.find("meta", property="og:image"):
        image = soup.find("meta", property="og:image")["content"]
    else:
        image = ""
    tmpdoc["image"] = image

    ## Gets the URIs
    url = soup.find("link", rel="canonical")["href"]
    tmpdoc["url"] = url

    ## Gets if expert reviewed
    if soup.find("a", {"class" : "sp_intro_expert"}):
        expert_reviewed = True
    else:
        expert_reviewed = False
    tmpdoc["expert"] = expert_reviewed

    ## Gets the intro text
    intro = soup.find(id="intro").find_all("p")[-1].text
    intro = re.sub(re.compile("\[\d+\]"), "", intro) # removes reference
    tmpdoc["intro"] = intro

    points = soup.find_all("b", {"class" : "whb"})
    if len(points) < 4:
        tmpdoc["points"] = ""
    else:
        points_list = list()
        points = [x.text for x in points]
        points = ["<li> - " + x + "</li>" for x in points]
        points = "".join(points)
        tmpdoc["points"] = points
    print("\n")
    cache.append(tmpdoc)

if __name__ == "__main__":
    Parser.run()
    Parser.create_output(cache)
    Parser.write_output_document(output)
    print("fin.")
