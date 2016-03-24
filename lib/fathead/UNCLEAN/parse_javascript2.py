from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import urllib2

openclosediv = re.compile('''<div.*?>|</div>''',re.DOTALL)
openclosep = re.compile('''<p.*?>|</p>''',re.DOTALL)
opencloseh3 = re.compile('''<h3.*?>|</h3>''',re.DOTALL)
openclosett = re.compile('''<t.*?>|</tt>|<tt>|</td>|</tr>''',re.DOTALL)

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

for file in os.listdir('./docs/javascript/'):
  if '.asp' in file:
    files.append('./docs/javascript/%s'%(file))


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)

  t = soup.findAll(attrs={"style":"background-color:#ffffff;color:#000000;padding-bottom:8px;padding-right:5px"})

  url = "http://www.w3schools.com/jsref/%s"%(file.replace('./docs/javascript/',''))
  name = t[0].findAll("h2")[0].string.split(' ')[0]
  desc = t[0].findAll("p")[0].string

  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,'','','javascript','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,'','','javascript','en')


  for table in soup.findAll(attrs={"class":"reference"}):
    for tr in table.findAll("tr"):
      td = tr.findAll("td")
      if len(td) != 0:
        try:
          one = td[0].findAll('a')[0]
          namespace = name
          methname = one.string.replace('()','')
          url = "http://www.w3schools.com/jsref/%s"%( one['href'])
          desc = td[1].string

          opener = urllib2.build_opener()
          url_opener = opener.open(url)
          page = url_opener.read()
          soup2 = BeautifulSoup(page)
          synopsis = openclosetags.sub('',str(soup2.findAll(attrs={"class":"code notranslate"})[0])).strip()

          if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
            print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(methname,namespace,url,desc,synopsis,'','javascript','en')
          if sys.argv[1].lower() == 'sql':
            print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(methname,namespace,url,desc,synopsis,'','javascript','en')
        except:
          pass
