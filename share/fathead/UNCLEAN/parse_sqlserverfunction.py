from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/sqlserver/functions/'):
  if '.html' in file:
    files.append('./docs/sqlserver/functions/%s'%(file))

for file in files:
  filecontents = ''
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents = "%s %s"%(filecontents,line.strip())

  soup = BeautifulSoup(filecontents)

  name = soup.findAll('h1')[0].string.replace('(Transact-SQL)','')
  desc = openclosetags.sub('',str(soup.findAll(attrs={"class" : "introduction"})[0].findAll('p')[0]))
  synopsis = soup.findAll(attrs={"class":"LW_CodeSnippetContainerCodeCollection"})[0].findAll('pre')[0].string.strip()

  url = "http://msdn.microsoft.com/en-us/library/%s"%(file.replace('./docs/sqlserver/functions/library','').replace('.html',''))
  url = url.replace('./docs/sqlserver/functions/','')

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','sqlserverfunction','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'sql server sqlserver sqlserver2008 2008','sqlserverfunction','en')
