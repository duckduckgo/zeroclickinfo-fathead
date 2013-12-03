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

for file in os.listdir('./docs/svn/'):
  if 'svn.ref.svn.c' in file and '.html' in file:
    files.append('./docs/svn/%s'%(file))

for file in os.listdir('./docs/svn/'):
  if 'svn.ref.svnadmin.c' in file and '.html' in file:
    files.append('./docs/svn/%s'%(file))


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"class" : "refnamediv"})
  name = spaces.sub(' ',openclosetags.sub('',openclosep.sub('',t[0].findAll('p')[0].prettify()).strip()))
  searchname = ' '.join(name.split(' ')[:2])
  synopsis = str(soup.findAll('pre')[0])
  synopsis = openclosetags.sub('',synopsis)
  url = "http://svnbook.red-bean.com/en/1.5/%s"%(file.replace('./docs/svn/',''))

  t = soup.findAll(attrs={"class":"refsect1"})
  description = ''
  for p in t[1].findAll('p'):
    description = "%s %s"%(description,p)
  description = openclosetags.sub('',description)

  t = soup.findAll(text=re.compile('Options'))
  if len(t) != 0:
    if str(t[0].next.string) != 'None':
      synopsis = "%s %s"%(synopsis,str(t[0].next.string))

  t = soup.findAll(text=re.compile('Alternate names'))
  if len(t) != 0:
    if openclosetags.sub('',str(t[0].next)) != 'None':
      previoushasseperator = True
      for altname in openclosetags.sub('',str(t[0].next)).split(' '):
        altname = altname.replace(',','')
        p = searchname.split(' ')
        p[1] = altname
        altname = ' '.join(p)

        if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
          print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(altname,'',url,name,synopsis,description,'svn','en')
        if sys.argv[1].lower() == 'sql':
          print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(altname,'',url,name,synopsis,description,'svn','')
        if ',' in altname:
          previoushasseperator = True
        else:
          break



  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(searchname,'',url,name,synopsis,description,'svn','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(searchname,'',url,name,synopsis,description,'svn','')

