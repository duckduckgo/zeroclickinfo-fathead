#!/usr/bin/env python2

from BeautifulSoup import BeautifulSoup

class Parser(object):
    def __init__(self, input='download/About:config_entries'):
        self.soup = BeautifulSoup(open(input))

    def findEntries(self):
        self.tags = []
        table = self.soup.findAll('div', id="bodyContent")[0]
        for table in table.findAll('table'):
            header = True
            for tr in table.findAll('tr'):
                if header:
                    header = False
                    continue
                i = 0
                print
                for th in tr.findAll('td'):
                    if i == 0: print "name: ", th.b.contents
                    elif i == 1: print "value: ", th.contents
                    elif i == 2:
                        print "description: ", th.contents
                        i = 0; continue
                    i += 1


if __name__ == "__main__":
    parser = Parser()
    parser.findEntries()
