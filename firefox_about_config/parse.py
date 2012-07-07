#!/usr/bin/env python2

from BeautifulSoup import BeautifulSoup
import urllib
import string
import re


class Entry(object):
    def __init__(self, name, value, description):
        self.name = name
        self.value = value
        self.description = description
        self.url = 'http://kb.mozillazine.org/About:config_entries'

    def __str__(self):
        self.url += "#" + string.capitalize(urllib.quote(self.name.split('.')[0])) + "."
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
                    elif i == 1:
                        value = th.text
                    elif i == 2:
                        if value:
                            article = 'a'
                            if value[0] == 'I': article += 'n'
                            optionType = "It accepts " + article + " " + value + " value."
                        synopsis = name + ' is a configuration option ' \
                                'for the Firefox web browser. ' + optionType
                        for element in th.contents:
                            try:
                                description += " " + element
                            except TypeError: 
                                description += str(element)
                        if ''.join(description.split()) != '':
                            if name == 'bidi.edit.deleteimmediately':
                                print description
                            description = '<pre>' + description.replace('\n', '<br>').strip()
                            expandedURL = 'href="' + self.baseURL
                            description = description.replace('href="/', expandedURL)
                            description = re.sub('<\s*b\s*>', '<i>', description)
                            description = re.sub('<\s*/\s*b\s*>', '</i>', description)
                            description += '</pre>'
                        description = synopsis + description
                        i = -1
                        self.entries.append(Entry(name, value, description.strip()))
                    i += 1


if __name__ == "__main__":
    parser = Parser()
    parser.findEntries()
    with open('output.txt', 'w') as file:
        for entry in parser.entries:
            file.write(entry.__str__().encode('UTF-8') + '\n')
