from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

files.append('./docs/NginxHttpCoreModule.htm')
#files.append('./NginxHttpCoreModule.htm')

def geth2locations(filecontents):
  ret = []
  count = 0
  for x in filecontents:
    count += 1
    if 'h2' in x:
      ret.append(count)
  ret.append(count)
  return ret

for file in files:
  filecontents = []
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents.append(line.strip())
  
  contents = geth2locations(filecontents)
  for x in range(len(contents)-1):
    name = ''
    synopsis = ''
    description = ''
    soup = BeautifulSoup(''.join(filecontents[contents[x]-1:contents[x+1]-1]))
    h2 = soup.findAll('h2')
    if len(h2) != 0:
      name = openclosetags.sub('',str(h2[0])).strip()
    ps = soup.findAll('p')
    if '$' in name:
      description = openclosetags.sub('',str(ps[0]))
    else:
      for p in ps:
        if '<b>' in str(p):
          synopsis = '%s<br>%s'%(synopsis,openclosetags.sub('',str(p)))
        else:
          description = openclosetags.sub('',str(p))
          break
    url = 'http://wiki.nginx.org/NginxHttpCoreModule#%s'%(name.replace('$','.24'))
    if name != '':
      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,description,synopsis[4:],'','nginxcoremodule','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,description.replace("'","\\'"),synopsis.replace("'","\\'"),'nginx core module','nginxcoremodule','en')
