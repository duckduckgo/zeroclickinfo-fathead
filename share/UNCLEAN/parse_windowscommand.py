from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/windowscommand/'):
  if '.html' in file:
    files.append('./docs/windowscommand/%s'%(file))

for file in files:
  filecontents = ''
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents = "%s %s"%(filecontents,line.strip())

  soup = BeautifulSoup(filecontents)

  name = openclosetags.sub('',str(soup.findAll(attrs={"class":"title"})[0]))
  p = soup.findAll(attrs={"id":"mainBody"})[0].findAll('p')
  desc = openclosetags.sub('',str(p[0])).strip()
  if desc == '':
    desc = openclosetags.sub('',str(p[1])).strip()

  synopsis = ""
  for p in soup.findAll('pre'):
    if name.lower() in str(p).lower():
      synopsis = openclosetags.sub('',str(p))
      break
  
  url = "http://technet.microsoft.com/en-us/library/%s"%(file.replace('./docs/windowscommand/library','').replace('.html',''))

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','windowscommand','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'windows command commandline line','windowscommand','en')
