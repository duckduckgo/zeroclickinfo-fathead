from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

def findindex(haystack,needle):
  count = 0
  for line in haystack:
    if needle in line:
      return count
    count += 1

def getsection(file,start,end):
  html = ''
  for i in file[start:end]:
    html = "%s\r\n%s"%(html,i)
  return html.strip()

def getall(file):
  html = ''
  for i in file:
    html = "%s\r\n%s"%(html,i)
  return html


openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/freebsdman/'):
  #if "git" in file:
  files.append('./docs/freebsdman/%s'%(file))


for file in files:
  filecontents = open(file).read()
  soup = BeautifulSoup(filecontents)


  lines = []
  if len(soup.findAll('pre')) == 0:
    continue
  for line in str(soup.findAll('pre')[0]).split("\n"):
    lines.append(line.replace("'",""))

  name = ''.join(openclosetags.sub('',getsection(lines,findindex(lines,'NAME'),findindex(lines,'SYNOPSIS'))).split("\n")[1:]).replace('...','').strip()
  name = name.replace("\r\n"," ")
  synopsis = openclosetags.sub('',getsection(lines,findindex(lines,'SYNOPSIS'),findindex(lines,'DESCRIPTION')))
  synopsis = synopsis.replace("\r\n","\n").replace("'","\'").replace("SYNOPSIS","")
  desc = openclosetags.sub('',getsection(lines,findindex(lines,'DESCRIPTION'),findindex(lines,'AUTHOR')))
  desc = "%s..."%(desc.replace("\r\n","\n").replace("'","\'").replace("DESCRIPTION","")[:850])
  url = "http://www.freebsd.org/cgi/%s"%(file.replace('./docs/freebsdman/',''))

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis,'','freebsdman','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,'freebsd man bsd','freebsdman','en')

