#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-
#
# A Fathead for listing various PEP (Python Enhancement Proposals) by their
# ID. PEPs exist for proposing new features into the language, collecting
# feedback from the community and documenting various design decisions.
# More Info: https://duck.co/ia/view/pep

__MODULE__ = "PEP Fathead"
__AUTHOR__ = "DDH Community"
__SOURCE__ = "https://www.python.org/dev/"
__PYBASE__ = "https://www.python.org"
__OUTPUT__ = "output.txt"

import os
import re
import sys
from urllib import request

from bs4 import BeautifulSoup

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
{information}
</section>
""".replace("\n", "")

class Preprocess(object):
    """
    The Prepocess object is responsible for setting up the directory and
    retrieving the relevant files. It consists of 3 member functions:

        - setup() - resets the environment (if needed)
        - open_pep_index() - retrieves PEP 0 which lists all PEPs
        - get_pep_urls() - gets all the PEP URLs from the PEP 0 HTML
    """
    links = set() # Empty set to hold the PEP URIs

    def __init__(self):
        self.setup()
        self.get_pep_urls()

    def setup(self):
        """
        If the output file exists, delete it and start over.
        """
        if os.path.exists(__OUTPUT__):
            os.remove(__OUTPUT__)

    def open_pep_index(self):
        """
        The open_pep_index function opens the byte stream of the PEP 0 HTML
        document found in the downloads/ directory.
        """
        with open("download/pep0.html", "rb") as f:
            content = f.read()
        return content

    def get_pep_urls(self):
        """
        This function parses PEP 0 and retrieves all the relevant URLs containing
        the various PEPs. It adds all the links with a PEP identifier to a set
        called links. A set is used to filter out any duplicates and read
        efficiency.

        A conditional is added to the end of this function to check that this
        was successful. If it wasn't, the program exits. Consider checking
        internet connection if this fails.
        """
        soup = BeautifulSoup(
                    self.open_pep_index(),
                    "html.parser",
                    from_encoding="UTF-8"
                )

        for link in soup.findAll('a', href=True, text=re.compile("^[0-9]*$")):
            self.links.add(link)

class DocumentParser(object):
    """
    The DocumentParser class is responsible for parsing the various HTML
    documents and extracting the relevant Introduction or Abstract information.

    A collection (set) is used to hold the final documents that are created by
    an instanciated object.

        - get_pep_contents() - gets the html from the file
        - set_pep_document() - extracts the appropriate information for the body
        - set_pep_title() - gets the title from the document
        - set_pep_number() - gets the PEPs number
        - set_pep_body() - gets the contents for the PEP
        - set_structure() - sets the structure for the file
    """
    collection = set()
    document = None
    pep = None
    title = None
    header = None
    body = None
    url = None

    def __init__(self, links):
        for link in links:
            self.get_pep_contents(link)
            self.set_pep_number()
            self.set_pep_title()
            self.set_pep_body()
            self.set_structure()
            print("Parsed " + self.pep)

    def get_pep_contents(self, link):
        """
        Gets the contents from the HTML file.
        """
        self.url = __PYBASE__ + link['href']
        html = request.urlopen(self.url)
        self.set_pep_document(html.read().decode("utf-8"))

    def set_pep_document(self, doc):
        """
        Makes up the HTML document.
        """
        self.document = BeautifulSoup(doc, "html.parser")

    def set_pep_title(self):
        """
        Gets the title for the PEP
        """
        # peptitle = self.document.find("h1", { "class": "page-title" }).get_text()
        # self.title = re.sub("--", "-", peptitle)
        self.title = "PEP " + self.pep

    def set_pep_number(self):
        """
        Sets the PEP number. For example, if the PEP is identified as PEP 0008,
        it will be stored as 8. If the PEP is identified as PEP 0143 it will be
        stored as 143.
        """
        pep = re.findall(r'\d+', self.url)[0]
        self.pep = pep.lstrip("0")

    def set_pep_body(self):
        """
        Parses the raw document, cleans the text
        """
        try:
            html = self.document.find("div", { "id": [
                    "abstract",
                    "introduction",
                    "rationale",
                    "motivation",
                    "what-is-a-pep",
                    "overview",
                    "improving-python-zip-application-support",
                    "scope",
                    "abstract-and-rationale",
                    "rationale-and-goals",
                    "specification"
                ]
            })

            if html is None: raise AttributeError

            html = FHTEMPLATE.format(information=str(html))
            html = html.replace("<pre class=\"literal-block\">", "<pre><code>")
            html = html.replace("</pre>", "</code></pre>")
            html = html.replace("<tt class=\"docutils literal\">", "<code class=\"inline\">")
            html = html.replace("</tt>", "</code>")
            html = re.sub(re.compile("<h1>((.|\n)+?)</h1>"), "", html)
            html = re.sub(re.compile("<a (.+?)>"), "", html)
            html = re.sub(re.compile("</a>"), "", html)
            html = re.sub(re.compile("\[\d+\]"), "", html) # removes the vancouver type referencing
            html = re.sub(re.compile("\n"), " ", html)
            html = html.rstrip().replace("\t", "\\t")
            self.body = html
        except AttributeError:
            pass
        except:
            print("Parse Error: Investigate " + self.pep)

    def set_structure(self):
        """
        Sets the structure for the output.txt file
        """
        if int(self.pep) not in [205, 210, 308]: # throw out redundent peps
            entry = OUTPUT_TEMPLATE.format(
                title=self.title,
                entry_type="A",
                redirect_title="",
                empty_field="",
                categories=self.pep,
                related_topics="",
                external_links="",
                disambiguation="",
                image="",
                abstract=self.body,
                url=self.url
            )
            self.collection.add(entry)

class OutputFile(object):
    """
    The object responsible for outputting data into the output.txt file.
    """
    def __init__(self, collection):
        with open(__OUTPUT__, "w") as f:
            for item in collection:
                f.write(item + "\n")

if __name__ == "__main__":
    # Preprocess > DocumentParser > OutputFile
    print("Starting " + __MODULE__)
    env = Preprocess()
    doc = DocumentParser(env.links)
    OutputFile(doc.collection)
    print("Finishing " + __MODULE__)
