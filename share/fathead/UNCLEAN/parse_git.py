from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosediv = re.compile('''<div.*?>|</div>''',re.DOTALL)
openclosep = re.compile('''<p.*?>|</p>''',re.DOTALL)
opencloseh3 = re.compile('''<h3.*?>|</h3>''',re.DOTALL)
openclosett = re.compile('''<t.*?>|</tt>|<tt>|</td>|</tr>''',re.DOTALL)


files = []

for file in os.listdir('./docs/git/html/'):
  if 'git-' in file and '.html' in file:
    files.append('./docs/git/html/%s'%(file))


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)
  
  t = soup.findAll(attrs={"class" : "sectionbody"})

  searchname = file.replace('./docs/git/html/','').replace('.html','').replace('-',' ')

  url = "http://schacon.github.com/git/%s"%(file.replace('./docs/git/html/',''))

  name = str(t[0])
  name = openclosep.sub('',openclosediv.sub('',name)).strip()

  synopsis = str(t[1])
  synopsis = openclosep.sub('',openclosediv.sub('',synopsis)).strip()

  description = str(t[2])
  description = openclosett.sub('',opencloseh3.sub('',openclosep.sub('',openclosediv.sub('',description)))).strip()
  description = description.replace('''<a href="''','''<a href="http://www.kernel.org/pub/software/scm/git/docs/''')

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(searchname,'',url,description.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','git','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(searchname,'',url,name,synopsis,description,'git','en')
