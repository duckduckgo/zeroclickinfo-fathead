from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)
cleaner = re.compile('\W',re.DOTALL)

files = []

for file in os.listdir('./docs/RFC-all/'):
  if '.txt' in file:
    files.append('./docs/RFC-all/%s'%(file))

for file in files:
  filecontents = open(file).read()

  name = file.replace('.txt','').split('/')[-1]
  url = 'http://tools.ietf.org/html/%s'%(name)
  name = name.upper()
  synopsis = ''
  
  split = filecontents.split("\n\n")
  split = map(lambda x: cleaner.sub(' ',x), split)
  split = map(lambda x: spaces.sub(' ',x).strip(), split)
  split = map(lambda x: x.replace('_',''),split)
  
  try:
    desc = filter(lambda x: len(x) >= 120, split)[0]
  except:
    desc = filter(lambda x: len(x) >= 0, split)[0]
  
  filecontents = spaces.sub(' ',cleaner.sub(' ',filecontents)).replace("'",'')
  
  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','rfc','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,filecontents,'rfc','en')
