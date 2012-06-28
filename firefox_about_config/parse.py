#!/usr/bin/env python2

from BeautifulSoup import BeautifulSoup


class Entry(object):
    def __init__(self, name, value, description):
        self.name = name
        self.value = value
        self.description = description
        self.url = 'https://github.com/duckduckgo/zeroclickinfo-fathead'

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
                    if i == 0:
                        name = ''.join(th.b.findAll(text=True)).replace(' ','')
                    elif i == 1:
                        value = th.text
                    elif i == 2:
                        description = ''
                        for element in th.contents:
                            try:
                                description += " " + element
                            except TypeError: 
                                description += str(element)
                        description = description.replace('\n', '\\n').strip()
                        i = -1
                        self.entries.append(Entry(name, value, description.strip()))
                    i += 1


if __name__ == "__main__":
    parser = Parser()
    parser.findEntries()
    with open('output.txt', 'w') as file:
        for entry in parser.entries:
            file.write(entry.__str__().encode('UTF-8') + '\n')
