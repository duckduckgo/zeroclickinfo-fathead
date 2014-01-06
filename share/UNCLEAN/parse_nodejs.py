from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import MySQLdb


def getall(soup):
  s = soup.nextSibling
  if s != None and '<h3' not in str(s):
    print ' '.join([str(s),getall(s)])
  return ''


conn = MySQLdb.connect(user='root')

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/nodejs/'):
  files.append('./docs/nodejs/%s'%(file))


for file in files:
  if 'buffers' not in file:
    continue
  filecontents = open(file).read()
  
  soup = BeautifulSoup(filecontents)
  
  for node in soup.findAll("h3"):
    print "=================================================="
    print openclosetags.sub('',str(node))
    #print node.next.next.next.next.next
    print getall(node)
    #print node.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling.nextSibling
    continue
    name = openclosetags.sub('',str(node.findAll("div","post-title")[0]))
    desc = openclosetags.sub('',str(node.findAll("div","p-con")[0].findAll('p')[0]))
    s = node.findAll("div","wp_syntax")[0].findAll('pre')
    synopsis = ''
    if len(s) == 1:
        synopsis = openclosetags.sub('',str(s[0]))
    else:
        synopsis = openclosetags.sub('',str(s[1]))
    url = node.findAll('a')[0]['href']


    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s"%(name,url,desc,synopsis,desc)
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,conn.escape_string(desc),conn.escape_string(synopsis),'','stuntsnippets','')
