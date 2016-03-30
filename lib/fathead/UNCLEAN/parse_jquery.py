from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
findjqueryscript = re.compile('''&lt;script&gt;.*?&lt;/script&gt;''',re.DOTALL)

files = []

for file in os.listdir('./docs/jquery/'):
  files.append('./docs/jquery/%s'%(file))

for file in files:
  filecontents = ''
  filecontents = open(file).read()
  filecontents = ''.join(filter(lambda x:x in string.printable, filecontents))


  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"class":"entry-content"})
  if len(t) == 0:
    #print file # dont want these ones
    continue

  t = t[0]

  name = t.findAll('h1')[0].string

  desc = openclosetags.sub('',str(t.findAll(attrs={"class":"desc"})[0]).replace("<strong>Description: </strong>",""))
  try:
    desc = "%s %s"%(desc,openclosetags.sub('',str(t.findAll(attrs={"class":"longdesc"})[0].findAll('p')[0])))
  except:
    pass

  synopsis = ''
  try:
    synopsis = openclosetags.sub('',str(t.findAll(attrs={"id":"example-0"})[0].findAll('pre')[0]))
    synopsis = findjqueryscript.findall(synopsis)[0]
  except:
    pass

  url = "http://api.jquery.com/%s/"%(file.replace("./docs/jquery/","").replace(".html","").replace(".htm",""))
  

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc.replace("\n","__NEWLINE__").replace("\r\n","__NEWLINE__").replace("\t","    "),synopsis.replace("'","''").replace("\n","__NEWLINE__").replace("\r\n","__NEWLINE__").replace("\t","    "),'','jquery','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc.replace("'","''"),synopsis.replace("'","''"),'','jquery','en')
