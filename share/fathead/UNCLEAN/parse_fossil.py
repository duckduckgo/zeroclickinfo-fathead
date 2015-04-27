from BeautifulSoup import BeautifulSoup
import re
import os
import sys

def findindex(t,find):
  count = 0
  for i in t:
    if find in i:
      return count
    count +=1
  return -1

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/fossil/'):
  files.append('./docs/fossil/%s'%(file))


for file in files:
  filecontents = open(file).read()
  

  soup = BeautifulSoup(filecontents)
  
  name = str(soup.findAll('h1')[0]).replace('<h1>The "','').replace('" command:</h1>','')
  url = "http://www.fossil-scm.org/index.html/help?cmd=%s"%(name)
  description = ''
  t = openclosetags.sub('',str(soup.findAll('pre')[0])).split("\n")
  synopsis = t[findindex(t,'Usage: ')].replace('Usage: ','')
  if synopsis == '':
    synopsis = (''.join(t[0:2])).strip()
  
  for i in range(3,len(t)):
    if t[i] == '' and i != 3:
      description = (' '.join(t[3:i])).strip()
      break

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,description.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','fossil','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name.replace("'","''"),'',url,description.replace("'","''"),synopsis.replace("'","''"),'','fossil','en')
