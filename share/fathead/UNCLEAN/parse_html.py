from BeautifulSoup import BeautifulSoup
import re
import os
import sys


openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/html/'):
  files.append('./docs/html/%s'%(file))

  #id class style

for file in files:
  filecontents = open(file).read()
  
  soup = BeautifulSoup(filecontents)
  
  name = openclosetags.sub('',str(soup.findAll("h1")[0]))
  namespace = ''
  synopsis = openclosetags.sub('',str(soup.findAll("table")[0].findAll('td')[0]))
  synopsis += "\r\n\r\nAttributes"
  
  for li in soup.findAll('ul')[0].findAll('li'):
    li = openclosetags.sub('',str(li))
    if li != "common attributes":
      synopsis += "\r\n" + li
    else:
      synopsis += "\r\nID" 
      synopsis += "\r\nCLASS" 
      synopsis += "\r\nSTYLE" 
  desc =  openclosetags.sub('',str(soup.findAll("p")[0]))
  
 
  url = "http://www.htmlhelp.com/reference/html40/%s"%(file.replace('./docs/html/',''))

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s"%(name,url,desc,synopsis,desc)
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,namespace,url,desc.replace("'","''"),synopsis.replace("'","''"),'','html','')
