import os
import re
import codecs
import sys

r = re.compile('''<p class="simpara">.*?</p>''')
r3 = re.compile('''<p class="para">.*?</p>''')
r4 = re.compile('''<div class="cdata">.*?</div>''')
r5 = re.compile('''<div class="methodsynopsis dc-description">.*?</div>''')
r2 = re.compile('''<.*?>''')


first = True

#langs = ['de','en','es','fa','fr','ja','pl','pt','ro','tr']
langs = ['en']

for lang in langs:
  dirList=os.listdir("./docs/phpapi/%s/"%(lang))
  for fname in dirList:
    if fname.startswith('function.') or fname == 'images':
      pass
    else:
      filelines = ''
      name = ''
      description = ''
      synopsis = ''
      for line in codecs.open("./docs/phpapi/%s/%s"%(lang,fname)):
        line = line.replace("'","")
        line = line.strip()
        if '<title>' in line:
          name = line.replace('<title>','').replace('</title>','').strip()
          name = r2.sub('',name).strip()

        filelines = "%s%s"%(filelines,line)
      t = r.findall(filelines)
      if len(t) != 0:
        description = t[0].replace('<p class="simpara">','').replace('</p>','')
      else:
        t = r3.findall(filelines)
        if len(t) != 0:
          description = t[0].replace('<p class="para">','').replace('</p>','')
    
    
      t = r5.findall(filelines)
      if len(t) != 0:
        synopsis = t[0].replace('<div class="cdata">','').replace('</div>','').replace('<pre>','').replace('</pre>','')
      else:
        t = r4.findall(filelines)
        if len(t) != 0:
          synopsis = t[0].replace('<div class="cdata">','').replace('</div>','').replace('<pre>','').replace('</pre>','')

      if 'foreach' in fname or 'while' in fname or 'for' in fname or 'declare' in fname:
        synopsis = synopsis
      else:
        synopsis = ''

      fname = "%s.php"%(fname[:-5])
      toprint = ''

      description = description.replace('<code>','<pre>').replace('</code>','</pre>')
      url = 'http://www.php.net/manual/en/%s'%(fname)

      if 'control-structures' in fname and fname != 'language.control-structures.php' and fname != 'control-structures.intro.php':
        if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
          print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,description,synopsis,filelines,'php','en')
        if sys.argv[1].lower() == 'sql':
          print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,description,synopsis,name.replace('_',' '),'php','en')

