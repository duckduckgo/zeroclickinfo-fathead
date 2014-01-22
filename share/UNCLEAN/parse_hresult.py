from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosediv = re.compile('''<div.*?>|</div>''',re.DOTALL)
openclosep = re.compile('''<p.*?>|</p>''',re.DOTALL)
opencloseh3 = re.compile('''<h3.*?>|</h3>''',re.DOTALL)
openclosett = re.compile('''<t.*?>|</tt>|<tt>|</td>|</tr>''',re.DOTALL)

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)


files = []

files.append('./docs/microsoft/hresult.html')


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)
  t = soup.findAll(attrs={"class":"FixedWidth-40-60"})
  for i in t[0].findAll("tr"):
    j = i.findAll("td")
    if len(j) != 0:
      name1 = openclosetags.sub(' ',str(j[0])).strip().split(' ')[0]
      name2 = openclosetags.sub(' ',str(j[0])).strip().split(' ')[1]
      name = "%s %s"%(name1,name2)
      desc = openclosetags.sub(' ',str(j[1])).strip()

      url = '''http://msdn.microsoft.com/en-us/library/cc704587%28v=PROT.10%29.aspx'''

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,'','','hresult','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,'','','hresult','en')
