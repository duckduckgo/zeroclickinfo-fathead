from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/apache2/'):
  if '.html' in file:
    files.append('./docs/apache2/%s'%(file))

for file in files:
  filecontents = ''
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents = "%s %s"%(filecontents,line.strip())

  soup = BeautifulSoup(filecontents)

  for dir in soup.findAll(attrs={"class":"directive-section"}):
    name = openclosetags.sub('',str(dir.findAll('h2')[0])).strip()
    desc = ''
    p = dir.findAll('p')
    if len(p) == 0:
      desc = openclosetags.sub('',str(dir.findAll(attrs={"class":"note"})[0]))
    else:
      desc = openclosetags.sub('',str(p[0]))
    synopsis = openclosetags.sub('',str(dir.findAll('tr')[1].findAll('td')[0]))

    url = "http://httpd.apache.org/docs/2.2/mod/%s#%s"%(file.replace('./docs/apache2/',''),dir.findAll('a')[0]['id'])

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','apache2directive','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'apache apache2 directive apache2.2','apache2directive','en')
