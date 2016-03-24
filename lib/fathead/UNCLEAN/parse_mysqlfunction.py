from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosediv = re.compile('''<div class="itemizedlist">.*?</div>|<div class="orderedlist">.*?</div>''',re.DOTALL)
openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/mysql/functions/'):
  if 'functions' in file and '.html' in file:
    files.append('./docs/mysql/functions/%s'%(file))

for file in files:
  filecontents = ''
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents = "%s %s"%(filecontents,line.strip())

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"class" : "itemizedlist"})[0]
  t = openclosediv.sub('',str(t.contents[0]))
  t = BeautifulSoup(t)

  for li in t.findAll('li'):
    name = openclosetags.sub('',str(li.findAll('p')[0])).strip()
    desc = openclosetags.sub('',str(li.findAll('p')[1])).strip()
    synopsis = ''
    for a in li.findAll('a'):
      try:
        url = a['href']
        break
      except:
        pass
      

    pre = li.findAll('pre')
    if len(pre) != 0:
      synopsis = openclosetags.sub('',str(pre[0])).replace('mysql&gt;',"\r\nmysql>").replace('','').strip()


    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis.replace("\r\n","__NEWLINE__"),'','mysqlfunction','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name.replace("'","\\'").replace("\\\\","\\"),'',url,desc.replace("'","\\'").replace("\\\\","\\"),synopsis.replace("'","\\'").replace("\\\\","\\"),'','mysqlfunction','en')
