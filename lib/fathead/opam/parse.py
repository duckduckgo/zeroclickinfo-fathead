#!/usr/bin/python

import os
import re
from urlparse import urljoin

from bs4 import BeautifulSoup

__TARGET__ = "https://opam.ocaml.org/"
__MODULE__ = "OPAM"


class Environment(object):
    """
    The Environment class is responsible for setting up the directory
    and retrieving the appropriate files from the web. It is made up of
    a constructor and a setup() and get_contents() member functions.
    """
    def __init__(self):
        print("Environment instantiated")
        self.setup()

    def setup(self):
        """
        The setup member function checks to see if the 'output.txt' file
        exists. If it does, delete it and move on, otherwise move on. This
        is called in the constructor of the Environment object and shouldn't
        be called again.
        """
        if os.path.exists("output.txt"):
            os.remove("output.txt")

    def get_contents(self):
        """
        The get_contents() member function loads the file into memory and
        reads the contents. The file buffer is then closed and then a
        BeautifulSoup object is instantiated, with the contents. More on
        this can be read at the BeautifulSoup documentation website
        (https://www.crummy.com/software/BeautifulSoup/bs4/doc/).
        """
        with open('download/packages.html') as f:
            contents = f.read()
        contents = BeautifulSoup(contents, 'html.parser', from_encoding="utf-8")

        return contents


class Document(object):
    """
    The Document class is responsible for setting up the end document
    (output.txt). It is made up of 4 member functions and a dummy
    constructor.
    """
    def __init__(self):
        print("Document object instantiated")

    def parse_contents(self, soup):
        """
        This member function parses the html document and extracts the
        cells from the table.
        """
        data = []
        table = soup.find('table', attrs={'id': 'packages'})
        table_body = table.find('tbody')
        rows = table_body.find_all('tr')

        """
        This section aims to extract the individual data points from the
        table and turn them into a list. For each of the columns found in
        <td> tags, we return them in a list, while finding one link.
        We then preprocess the contents by stripping whitespace and
        stripping the front of the relative link. Once this preprocessing
        has been done, we connect the URL, and append it to the list.
        """
        for row in rows:
            cols = row.find_all('td')
            ref = row.find('a', href=True)

            # Preprocessing
            cols = [ele.text.strip() for ele in cols]
            relurl = ref['href'].strip('../')

            # Add to list
            aburl = urljoin(__TARGET__, relurl)
            cols.append(aburl)
            print(cols)
            data.append(cols)

        return data

    def concat(self, name, desc, url, version):
        """
        The concat (concatenation) member function is responsible for
        preparing the data to be written to the file. The file is
        layed outlike this as requested in the DuckDuckHack docs found
        here: http://docs.duckduckhack.com/resources/fathead-overview.html#data-file-format
        """
        title = name
        type_ = 'A'
        redirect = ''
        four = ''  # IGNORE
        categories = ''
        six = ''  # IGNORE
        related_topics = ''
        eight = ''  # IGNORE
        external_links = ''
        ten = ''  # IGNORE
        image = ''
        nAbstract = u"""
            <section class="prog__container">
                <p>{0}</p>
                <pre>
                    <code>
                        opam install {1}
                    </code>
                </pre>
                <span class="prog__sub">Version: {2}</span>
            </section>
            """.format(desc, name, version)
        abstract = re.sub("\s+", " ", nAbstract)
        url = url

        data = [
            title,
            type_,
            redirect,
            four,
            categories,
            six,
            related_topics,
            eight,
            external_links,
            ten,
            image,
            abstract,
            url
        ]

        line = "\t".join(data) + "\n"

        return line

    def output(self, data_list):
        """
        The output() member function outputs the rows of data at a
        time to the output.txt file which is used as the k/v store
        for the FatHead IA.
        """
        with open('output.txt', 'a') as f:
            for data in data_list:
                line = self.concat(data[0], data[2], data[3], data[1])
                f.write(line.encode('utf'))

if __name__ == "__main__":
    print("Launching " +  __MODULE__)
    env = Environment()
    doc = Document()
    contents = env.get_contents()
    processed_contents = doc.parse_contents(contents)
    doc.output(processed_contents)
    print("Finishing " +  __MODULE__)
