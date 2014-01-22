from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/perl/functions/'):
#  if file == 'abs.html':
  files.append('./docs/perl/functions/%s'%(file))


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"id" : "content_body"})
  name = t[0].findAll('h1')[0].string

  syn = t[0].findAll('ul')
  synopsis = syn[0].findAll('b')[0].string
  if len(syn) == 2:
    for b in syn[1].findAll('b'):
      synopsis = "%s\r\n%s"%(synopsis,b.string)

  for ul in t[0].findAll('ul'):
    for p in ul.findAll('p'):
      if openclosetags.sub('',str(p)).strip() != '':
        desc = openclosetags.sub('',str(p)).strip()
        if desc[-1] == ':':
          desc = "%s..."%(desc[:-1])
        break
  url = 'http://perldoc.perl.org/%s'%(file.replace('./docs/perl/',''))

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','perl5','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,filecontents,'perl5','en')
