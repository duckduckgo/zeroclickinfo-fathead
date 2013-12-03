from BeautifulSoup import BeautifulSoup
import re
import os
import sys

files = []
files.append('./docs/httpcode')


for file in files:
  lines = []
  for line in open(file):
    line = line.replace("'","\\'")
    lines.append(line.strip())

  for i in range(len(lines)-1):
    if i%2 ==0:
      name = lines[i]
      desc = lines[i+1]

      url = 'http://en.wikipedia.org/wiki/List_of_HTTP_status_codes'

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,'','','httpcode','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, 

`lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,'','','httpcode','en')

