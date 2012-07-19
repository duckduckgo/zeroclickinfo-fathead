#!/usr/bin/env python2

from BeautifulSoup import BeautifulSoup, NavigableString
import urllib
import string
import re


class Entry(object):
    def __init__(self, name, value, description, url):
        self.name = name
        self.value = value
        self.description = description
        self.url = url

    def __str__(self):
        fields = [
                self.name,              # title
                'A',                    # type
                '',                     # redirect
                '',                     # otheruses
                '',                     # categories
                '',                     # references
                '',                     # see_also
                '',                     # further_reading
                '',                     # external_links
                '',                     # disambiguation
                '',                     # images
                self.description,       # abstract
                self.url                # source_url
                ]
        return '%s' % ('\t'.join(fields))


class Parser(object):
    def __init__(self, input='download/About:config_entries'):
        self.soup = BeautifulSoup(open(input))
        # Requires trailing / for relative link replacement
        self.baseURL = "http://kb.mozillazine.org/"

    def findEntries(self):
        self.entries = []
        headers = map(lambda x: x.string, self.soup.findAll('h1')[2:])
        table = self.soup.findAll('div', id="bodyContent")[0]
        for table in table.findAll('table'):
            header = True
            for tr in table.findAll('tr'):
                if header:
                    header = False
                    continue
                i = 0
                for th in tr.findAll('td'):
                    description = ''
                    if i == 0:
                        name = ''.join(th.b.findAll(text=True)).replace(' ','')
                        anchor = string.capitalize(urllib.quote(name.split('.')[0])) + "."
                        if anchor in headers:
                            url = self.baseURL + 'About:config_entries#' + anchor
                        else:
                            url = self.baseURL + 'About:config_entries'
                    elif i == 1:
                        value = th.text
                    elif i == 2:
                        if value:
                            article = 'a'
                            if value[0] == 'I': article += 'n'
                            optionType = "it accepts " + article + " " + value.lower() + "."
                        synopsis = '"' + name + '"'  + ' is a configuration option ' \
                                'for the Firefox web browser; ' + optionType + "<br>"
                        for tag in th.findAll('br'):
                            tag.insert(0, NavigableString("\n"))
                        description = ''.join(th.findAll(text=True))
                        description = description.rstrip().replace('\n', '<br>').strip()
                        expandedURL = 'href="' + self.baseURL
                        description = description.replace('href="/', expandedURL)
                        description = re.sub('<\s*b\s*>', '<i>', description)
                        description = re.sub('<\s*/\s*b\s*>', '</i>', description)
                        description = '<blockquote>' + description + '</blockquote>'
                        description = synopsis + description
                        i = -1
                        self.entries.append(Entry(name, value, description.strip(), url))
                    i += 1


if __name__ == "__main__":
    parser = Parser()
    parser.findEntries()
    with open('output.txt', 'w') as file:
        for entry in parser.entries:
            file.write(entry.__str__().encode('UTF-8') + '\n')
