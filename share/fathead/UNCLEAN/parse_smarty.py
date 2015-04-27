from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosediv = re.compile('''<div.*?>|</div>''',re.DOTALL)
openclosep = re.compile('''<p.*?>|</p>''',re.DOTALL)
opencloseh3 = re.compile('''<h3.*?>|</h3>''',re.DOTALL)
openclosett = re.compile('''<t.*?>|</tt>|<tt>|</td>|</tr>''',re.DOTALL)

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/smarty/'):
  if 'language.modifier.' in file:
    files.append('./docs/smarty/%s'%(file))

'''for file in os.listdir('./docs/smarty/'):
  if 'language.function.' in file:
    files.append('./docs/smarty/%s'%(file))'''


for file in files:
  filecontents = ''
  for line in open(file):
    line = line.replace("'","\\'").replace("\\\\'","\\'")
    filecontents = "%s %s"%(filecontents,line)

  soup = BeautifulSoup(filecontents)
  name = openclosetags.sub('',str(soup.findAll('h2')[0])).strip()
  desc = openclosetags.sub('',str(soup.findAll('p')[0])).strip()

  url = "http://www.smarty.net/docs/en/%s"%(file.replace('./docs/smarty/','').replace('.html','.tpl'))

  synopsis = "%s\r\n"%(openclosetags.sub('',str(soup.findAll(attrs={"class":"programlisting"})[0])).strip())
  syn = openclosetags.sub('',str(soup.findAll(attrs={"class":"programlisting"})[1])).strip()
  syn2 = openclosetags.sub('',str(soup.findAll(attrs={"class":"screen"})[0])).strip()

  synsplit = syn.split("\n")
  synsplit2 = syn2.split("\n")

  largest = max(map(lambda x:len(x),synsplit))

  if len(synsplit) == len(synsplit2):
    for i in range(len(synsplit)):
      synopsis = "%s\r\n%s - %s"%(synopsis,synsplit[i].strip().ljust(largest),synsplit2[i])
  else:
    synopsis = syn

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'smarty.%s'%(name),url,desc,synopsis,'','smarty','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'smarty.%s'%(name),url,desc,synopsis,'','smarty','en')
  


