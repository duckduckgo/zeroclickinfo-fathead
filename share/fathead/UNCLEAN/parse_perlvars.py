from BeautifulSoup import BeautifulSoup
import re
import os
import sys

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

for file in os.listdir('./docs/perl/perlvar/'):
  if '.html' in file:
    files.append('./docs/perl/perlvar/%s'%(file))


for file in files:
  filecontents = ''
  for line in open(file):
    filecontents = "%s %s"%(filecontents,line.strip())
    filecontents = filecontents.replace("'","")

  soup = BeautifulSoup(filecontents)

  for ul in soup.findAll("ul"):
    prevnames = []
    for li in ul.findAll('li',recursive=False):
      b = li.findAll('b')
      p = li.findAll('p')
      pre = li.findAll('pre')
      name = openclosetags.sub('',str(b[0]))
      synopsis = ""
      if len(p) == 0:
        prevnames.append(name)
      else:
        desc = openclosetags.sub('',str(p[0]))
        if len(pre) != 0:
          for l in pre[0].findAll('li'):
            synopsis = "%s\r\nb%s"%(synopsis,openclosetags.sub('',str(l)).strip())
        synopsis = synopsis.strip()
        synopsis = synopsis.replace("\r\n","\n")

        url = "http://perldoc.perl.org/perlvar.html#%s"%(li.findAll('a')[0]['name'])
        url = url.replace("\\","\\\\")
        prevnames.append(name)
        for name in prevnames:
          if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
            print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis.replace("\r\n","__NEWLINE__"),'','perl5var','en')

        if sys.argv[1].lower() == 'sql':
          name = ' '.join(prevnames)
          print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,'','perl5var','en')
        prevnames = []

