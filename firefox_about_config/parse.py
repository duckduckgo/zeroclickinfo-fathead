#!/usr/bin/env python2

from BeautifulSoup import BeautifulSoup

#print 'lol'.encode('utf-8')

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
                    if i == 0:
                        name = ''.join(th.b.findAll(text=True)).replace(' ','')
                        print 'name: ', name
                        file.write(name + '\t')
                    elif i == 1:
                        value = th.text
                        print 'value: ', value
                        file.write(value + '\t')
                    elif i == 2:
                        description = ''
                        for element in th.contents:
                            try:
                                description += " " + element
                            except TypeError: 
                                description += str(element)
                        print "description: ", description
                        file.write(description + '\n')
                        i = 0; continue
                    i += 1


if __name__ == "__main__":
    with open('output.txt', 'w') as file:
        parser = Parser()
        parser.findEntries()

