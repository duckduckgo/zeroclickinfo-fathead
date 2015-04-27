from BeautifulSoup import BeautifulSoup
import re
import os
import sys


openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)


files = []

files.append('./docs/mysql/error.html')


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)
  t = soup.findAll(attrs={"class":"itemizedlist"})
  for i in t[2].findAll("li"):

    p = i.findAll('p')
    name = openclosetags.sub('',str(p[0])).strip()
    desc = openclosetags.sub('',str(p[1])).strip()
    url = "http://dev.mysql.com/doc/refman/5.5/en/%s"%(i.findAll('a')[1]['href'])

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,'','','mysqlerror','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,'','','mysqlerror','en')
