from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []


files.append('./docs/ruby-doc-bundle/Manual/man-1.4/options.html')


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")


  details = []

  soup = BeautifulSoup(filecontents)

  dt = soup.findAll("dt")
  dd = soup.findAll("dd")

  previous = ''
  for i in range(len(dt)):
    name = 'ruby %s'%(openclosetags.sub('',str(dt[i])).strip())
    detail = openclosetags.sub('',str(dd[i])).strip()
    if detail == '':
      previous = name
    else:
      if previous != '':
        details.append([previous,detail])
        previous = ''
      details.append([name,detail])


  for i in details:
    url = 'http://rubysomthing.ruby/%s'%(file.replace('./docs/perl/',''))

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(i[0],'',url,i[1],'','','ruby','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(i[0],'',url,i[1],'','','ruby','en')
