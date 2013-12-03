from BeautifulSoup import BeautifulSoup
import re
import os
import sys


openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/clojure/'):
  files.append('./docs/clojure/%s'%(file))


for file in files:
  filecontents = open(file).read()
  
  soup = BeautifulSoup(filecontents)
  
  namespace = openclosetags.sub('',str(soup.findAll(attrs={"id":"long-name"})[0]))
  
  for node in soup.findAll(attrs={"id":"var-entry"}):
    name = openclosetags.sub('',str(node.findAll('h2')[0]))
    synopsis = openclosetags.sub('',str(node.findAll(attrs={"id":"var-usage"})[0]))
    desc =  openclosetags.sub('',''.join(map(lambda x:str(x),(node.findAll(attrs={"id":"var-docstr"})))))
    url = "http://clojure.github.com/clojure/%s#%s/%s"%(file.replace('./docs/clojure/',''),namespace,name)

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,namespace,url,desc.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','clojure','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,namespace,url,desc.replace("'","''"),synopsis.replace("'","''"),'','clojure','')
