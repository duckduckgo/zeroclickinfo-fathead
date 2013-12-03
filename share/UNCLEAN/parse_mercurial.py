from BeautifulSoup import BeautifulSoup
import re
import os
import sys


replaceopenclosediv = re.compile('''<div.*?>|</div>''',re.DOTALL)
replaceopenclosep = re.compile('''<p.*?>|</p>''',re.DOTALL)
replaceopencloseh3 = re.compile('''<h3.*?>|</h3>''',re.DOTALL)
replaceopenclosett = re.compile('''<t.*?>|</tt>|<tt>|</td>|</tr>''',re.DOTALL)

findopenclosediv = re.compile('''<div.*?</div>''')

findopenclosetag = re.compile('''<.*?>|</.*?>''',re.DOTALL)


files = []

files.append('./docs/mercurial/hg.1.html')
#files.append('./hg.1.html')


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'",'')

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"id" : "commands"})

  for i in t[0].findAll('div'):
    searchname = None
    if len(i.findAll('h2')) != 0:
      searchname = "hg %s"%(i.findAll('h2')[0].string)
      url = "http://www.selenic.com/mercurial/hg.1.html#%s"%(searchname)
    if len(i.findAll('pre')) != 0:
      synopsis = i.findAll('pre')[0].string.strip()
    if len(i.findAll('p')) != 0:
      description = i.findAll('p')[0].string

    option = ""
    if len(i.findAll(attrs={"class" : "docutils option-list"})) != 0:
      for tr in i.findAll(attrs={"class" : "docutils option-list"})[0].findAll('tr'):
        span = tr.findAll('td')[0].findAll('span')
        if len(span) >= 1:
          option = "%s %s"%(option,span[0].string)
        if len(span) == 2:
          option = "%s %s"%(option,span[1].string)

    if option != "":
      synopsis = "%s [Options: %s]"%(synopsis,option)

    if searchname != None:
      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(searchname,'',url,searchname,synopsis,description,'mercurial','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(searchname,'',url,description,synopsis,description,'mercurial','')

