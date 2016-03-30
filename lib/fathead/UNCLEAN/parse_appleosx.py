from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

#files.append('./docs/apple/osx/developer.apple.com.library/mac/documentation/Cocoa/Reference/NSCondition_class/Reference-Reference.html')


for root,dirs,filelist in os.walk('./docs/apple/osx/'):
  for file in filelist:
    if '.html' in file:
      files.append("%s/%s"%(root,file))


for file in files:
  filecontents = ''
  for line in open(file):
    line = ''.join(filter(lambda x:x in string.printable, line))
    filecontents = "%s %s"%(filecontents,line.strip())

  soup = BeautifulSoup(filecontents)

  # Get Object Details
  name = openclosetags.sub('',str(soup.findAll(attrs={"id":"pageTitle"})[0]))
  if len(soup.findAll(attrs={"class":"abstract"})) != 0:
    desc = openclosetags.sub('',str(soup.findAll(attrs={"class":"abstract"})[0]))
  else:
    temp = soup.findAll(attrs={"id":"Overview_section"})[0].findAll('p')
    temp = ''.join(map(lambda x:str(x),temp))
    desc = openclosetags.sub('',temp)

  name = name.split(' ')[0]
  url = "http://%s"%(file.replace('./docs/apple/osx/','').replace('\\','/').replace('developer.apple.com.','developer.apple.com/').replace('-','/'))
  synopsis = ''
  namespace = name


  if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,namespace,url,desc,synopsis,'','osx','en')
  if sys.argv[1].lower() == 'sql':
    print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,namespace,url,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'apple osx os x mac','osx','en')

  space = name
  for i in soup.findAll(attrs={"class":"api instanceMethod"}):
    name = i.findAll('h3')[0].string
    desc = openclosetags.sub('',str(i.findAll('p')[0]))
    namespace = "%s.%s"%(space,name)

    url2 = "%s#%s" %(url,i.findAll('a')[0]['name'])

    api = i.findAll(attrs={'class':'api discussion'})
    if len(api) != 0:
      desc = "%s %s"%(desc, openclosetags.sub('',' '.join(map(lambda x:str(x),api[0].findAll('p')))))
    if len(i.findAll(attrs={'class':'api availability'})) != 0:
      desc = '%s %s'%(desc,openclosetags.sub('',str(i.findAll(attrs={'class':'api availability'})[0].findAll('li')[0])))
    synopsis = openclosetags.sub('',str(i.findAll(attrs={'class':'declaration'})[0]))[2:]

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,namespace,url2,desc,synopsis,'','osx','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,namespace,url2,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'apple osx os x mac','osx','en')

  for i in soup.findAll(attrs={"class":"api classMethod"}):
    name = i.findAll('h3')[0].string
    desc = openclosetags.sub('',str(i.findAll('p')[0]))
    namespace = "%s.%s"%(space,name)

    url2 = "%s#%s" %(url,i.findAll('a')[0]['name'])

    api = i.findAll(attrs={'class':'api discussion'})
    if len(api) != 0:
      desc = "%s %s"%(desc, openclosetags.sub('',' '.join(map(lambda x:str(x),api[0].findAll('p')))))
    if len(i.findAll(attrs={'class':'api availability'})) != 0:
      desc = '%s %s'%(desc,openclosetags.sub('',str(i.findAll(attrs={'class':'api availability'})[0].findAll('li')[0])))
    synopsis = openclosetags.sub('',str(i.findAll(attrs={'class':'declaration'})[0]))[2:]

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,namespace,url2,desc,synopsis,'','osx','en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,namespace,url2,desc.replace("'","\\'"),synopsis.replace("'","\\'"),'apple osx os x mac','osx','en')


