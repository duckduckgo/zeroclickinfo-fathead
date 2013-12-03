from BeautifulSoup import BeautifulSoup
import re
import os
import sys

files = []
files.append('./docs/ftpcode.html')

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

for file in files:
  filecontents = ''
  for line in open(file):
    line = line.replace("'","\\'").strip()
    filecontents = '%s %s'%(filecontents,line)

  soup = BeautifulSoup(filecontents)
  for trtag in soup.findAll('tr'):
    td = trtag.findAll('td')
    if len(td) != 0:
      name = td[0].findAll('code')[0].string
      desc = openclosetags.sub('',str(td[1])).strip()

      url = 'http://en.wikipedia.org/wiki/List_of_FTP_server_return_codes'

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,'','','ftpcode','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, 

`lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,'','','ftpcode','en')

