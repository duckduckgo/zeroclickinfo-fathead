from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
openclosetagsem = re.compile('''<.[^em]*?>|</.[^em]*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/linuxcommand/'):
  files.append('./docs/linuxcommand/%s'%(file))



for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"id":"commanddirectory"})[0]

  name = t.findAll('h2')[0].string
  syn = t.findAll('p')
  synopsis =  openclosetagsem.sub('',str(syn[0])).strip()
  desc = openclosetags.sub('',str(syn[1])).strip()

  opt = t.findAll('variablelist')
  if len(opt) != 0:
    term = opt[0].findAll('term')
    options = ' '.join(map(lambda x:openclosetags.sub('',str(x)), term))
    synopsis = "%s [Options: %s]"%(synopsis,options)


  url = "http://oreilly.com/linux/command-directory/cmd.csp?path=%s/%s"%(name[0],name.replace('+','%2B'))


  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','linuxcommand','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,'','linuxcommand','en')