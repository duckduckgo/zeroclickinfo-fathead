import os
import re
import codecs
import sys


r = re.compile('''<div class="methodsynopsis dc-description">.*?</div>''')
r2 = re.compile('''<.*?>''')
r3 = re.compile('''<div class="refsect1 parameters">.*?</div>''')
r4 = re.compile('''<div class="refsect1 returnvalues">.*?</div>''')
r5 = re.compile('''<p class="simpara">.*?</p>''')
r6 = re.compile('''<p class="para">.*?</p>''')
r7 = re.compile('''<p class="verinfo">.*?<p class="refpurpose">.*?</p>''')


repdiv = re.compile('''<div.*?>''')
repdivc = re.compile('''</div>''')
repspan = re.compile('''<span.*?>''')
repspanc = re.compile('''</span>''')
reptt = re.compile('''<tt.*?>''')
repttc = re.compile('''</tt>''')
repspace = re.compile('''\s+''')

first = True

#langs = ['de','en','es','fa','fr','ja','pl','pt','ro','tr']
langs = ['en']

for lang in langs:
  dirList=os.listdir("./docs/phpapi/%s/"%(lang))
  for fname in dirList:
    if fname.startswith('function.'):
      filelines = ''
      name = ''
      description = ''
      synopsis = ''
      param = ''
      returnval = ''
      for line in codecs.open("./docs/phpapi/%s/%s"%(lang,fname)):
        line = line.replace("'","")
        line = line.strip()
        if '<h1 class="refname"' in line:
          name = line.replace('<h1 class="refname">','').replace('</h1>','').strip()
          name = r2.sub('',name).strip()

        if name == '' and '<h2 class="title"' in line:
          name = line.replace('<h2 class="title"><span class="function"><b>','').replace('</b></span></h2>','').replace('(','').replace(')','')

        if '<p class="refpurpose">' in line:
          description = line.replace('''<p class="verinfo">''','').replace('''</p><p class="refpurpose"><span class="refname">''',' ').replace('''<span class="refname">''',' ').replace('''</span>''',' ').replace('''<span class="dc-title">''',' ').replace('''</p>''','').strip()
          description = r2.sub('',description).strip()
        filelines = "%s%s"%(filelines,line)
      t = r.findall(filelines)
      if len(t) != 0:
        synopsis = t[0]

      t = r3.findall(filelines)
      if len(t) != 0:
        param = t[0]

      t = r4.findall(filelines)
      if len(t) != 0:
        returnval = t[0]

      t = r7.findall(filelines)
      if len(t) != 0:
        description = r2.sub('',t[0]).replace(')',') ')

      fname = "%s.php"%(fname[:-5])
      if description.strip() == '':
        t = r5.findall(filelines)
        if len(t) != 0:
          description = t[0]
      if description.strip() == '':
        t = r6.findall(filelines)
        if len(t) != 0:
          description = t[0]

      synopsis = repdiv.sub(' ',synopsis)
      synopsis = repdivc.sub(' ',synopsis)
      synopsis = repspan.sub(' ',synopsis)
      synopsis = repspanc.sub(' ',synopsis)
      synopsis = reptt.sub(' ',synopsis)
      synopsis = repttc.sub(' ',synopsis)
      synopsis = repspace.sub(' ',synopsis)

      url = 'http://www.php.net/manual/en/%s'%(fname)

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,description,synopsis,filelines,'php','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,description,synopsis,name.replace('_',' '),'php','en')
