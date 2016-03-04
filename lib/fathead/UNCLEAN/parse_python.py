from BeautifulSoup import BeautifulSoup
import os
import sys
import re


ver = 'python-2.7.1-docs-html'

findp = re.compile('''(<p>.*?</p>)|(<p.id.*?</p>)''',re.DOTALL)
findallstartp = re.compile('''<p.*?>''')
findalla = re.compile('''<a.*?</a>''',re.DOTALL)

finddlfun = re.compile('''<dl class="function">.*?</dl>''',re.DOTALL)
finddt = re.compile('''<dt .*?</dt>''',re.DOTALL)
finddd = re.compile('''<dd>.*?</dd>''',re.DOTALL)

findid = re.compile('''<dt id=".*?">''',re.DOTALL)
openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

for file in os.listdir('./docs/%s/library/'%(ver)):
  if '_' not in file and '2' not in file and file != 'crypto.html' and file != 'internet.html' and file != 'profile.html':# and file == 'crypt.html':
    filecontents = ''
    for line in open('./docs/%s/library/%s'%(ver,file)):
      line = line.replace("'","")
      line = line.strip()
      filecontents = "%s %s"%(filecontents,line)

    t = findp.findall(filecontents)
    desc = ''

    if len(t) != 0:
      if len(t[0][0]) >= 50:
        desc = t[0][0]
      else:
        if len(t) == 1:
          desc = t[0][1]
        else:
          if len(t[0][1]) != 0:
            desc = t[0][1]
          else:
            desc = t[1][0]
    if desc == '':
      desc = t[1][1]
    desc = findallstartp.sub('',desc).replace('</p>','')
    desc = openclosetags.sub('',desc)

    if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
      print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(file[:file.rfind('.')], '', "http://docs.python.org/library/%s"%(file), desc, '', '', 'python', 'en')
    if sys.argv[1].lower() == 'sql':
      print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(file[:file.rfind('.')], '', "http://docs.python.org/library/%s"%(file), desc, '', '', 'python', 'en')


    functions = finddlfun.findall(filecontents)
    for fun in functions:
      det = ''
      ded = ''
      dt = finddt.findall(fun)
      dd = finddd.findall(fun)
      if len(dt) != 0:
        det = dt[0]
        det = findalla.sub('',det)
      if len(dd) != 0:
        ded = dd[0].replace('href="','href="http://docs.python.org/library/%s'%(file))

      t = findid.findall(det)

      if len(t) != 0:
        fname = t[0].replace('<dt id="','').replace('">','')

        name = fname.split('.')[-1]
        namespace = fname
        url = "http://docs.python.org/library/%s#%s"%(file,fname)
        synopsis = openclosetags.sub('',det).strip()
        detail = BeautifulSoup(ded)
        if len(detail.findAll('p')) != 0:
          detail = openclosetags.sub('',str(detail.findAll('p')[0]))
        else:
          detail = openclosetags.sub('',ded)

        if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
          print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name, namespace, url, detail, synopsis, '', 'python', 'en')
        if sys.argv[1].lower() == 'sql':
          print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name, namespace, url, detail, synopsis, '', 'python', 'en')
