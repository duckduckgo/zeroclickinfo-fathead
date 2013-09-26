import os
import re
from bs4 import BeautifulSoup
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
  return html

def getall(file):
  html = ''
  for i in file:
    html = "%s\r\n%s"%(html,i)
  return html

r1 = re.compile('''<.*?>''',re.DOTALL)
findtr = re.compile('''<TR .*?>.*?</TR>''',re.DOTALL)
findtd = re.compile('''<TD>.*?</TD>''',re.DOTALL)
findtable = re.compile('''<TABLE .*?</TABLE>''',re.DOTALL)
findp = re.compile('''<P>.*?<P>''',re.DOTALL)
findpre = re.compile('''<PRE>.*?</PRE>''',re.DOTALL)
findh2 = re.compile('''<H2>.*?</H2>''',re.DOTALL)
findh3 = re.compile('''<H3>.*?</H3>''',re.DOTALL)
findcode = re.compile('''<code>.*?</code>''',re.DOTALL)
findcodeupper = re.compile('''<CODE>.*?</CODE>''',re.DOTALL)
findmethoddetail = re.compile('''<A NAME.*?<HR>''',re.DOTALL)
finda = re.compile('''<A NAME.*?>''',re.DOTALL)
findb = re.compile('''<B>.*?</B>''',re.DOTALL)
findddtop = re.compile('''<DD.*?<P>''',re.DOTALL)
findinherit = re.compile('''<B>Methods inherited from.*?</TABLE>''',re.DOTALL)
findopenclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

# java javax and org


#get all the files here
dirList = []


dir = "./docs/java/en/api/java/"

for (path,dirs,files) in os.walk(dir):
  if 'class-use' not in path:
    for f in files:
      dirList.append("%s/%s"%(path,f))

dir = "./docs/java/en/api/javax/"

for (path,dirs,files) in os.walk(dir):
  if 'class-use' not in path:
    for f in files:
      dirList.append("%s/%s"%(path,f))
  


first = True

for fname in dirList:
  file = []

  #if fname == 'XmlAnyElement.html':
  #if fname  == 'RandomAccess.html': # interface
  if fname.endswith('.html') and 'package-' not in fname and 'doc-files' not in fname:
    for line in open("%s"%(fname)):
      line = line.strip().replace("'",'')
      line = ''.join(filter(lambda x:x in string.printable, line))
      file.append(line)

    start   = findindex(file,"START OF CLASS DATA")
    consum  = findindex(file,"CONSTRUCTOR SUMMARY")
    methsum = findindex(file,"METHOD SUMMARY")
    condet  = findindex(file,"CONSTRUCTOR DETAIL")
    methdet = findindex(file,"METHOD DETAIL")
    end     = findindex(file,"END OF CLASS DATA")

    #finds the name and namespace
    np = findh2.findall(getall(file))[0]
    np = np.split('<BR>')
    namespace = r1.sub('',np[0]).strip()
    classtype = r1.sub('',np[1]).strip()

    #if its an interface skip it
    if 'interface' in classtype.lower():
      continue

    #finds the description which is the large text at the beginning
    desc = findp.findall(getall(file))[0]

    # print the object

    name = fname.split('/')[-1].replace('.html','')
    url = "http://download.oracle.com/javase/6/docs/%s"%(fname.replace('./docs/java/en/',''))
    description = spaces.sub(' ',findopenclosetags.sub('',desc).strip())


    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,namespace,url,description,'','','java','en')

    #finds all inherited methods
    for i in findinherit.findall(getall(file)):
      description = spaces.sub(' ',findopenclosetags.sub('',findb.findall(i)[0].replace('Methods','Method').replace('<B>','').replace('</B>','')))
      #print detail
      for j in findcodeupper.findall(i)[0].replace('<CODE>','').replace('</CODE>','').split('>, '):
        #synopsis = j.strip().replace('</A','</A>').replace('>>','>')
        synopsis = ''
        methodname =  r1.sub('',j).replace('</A','').strip()
        url = 'http://download.oracle.com/javase/6/docs/%s#%s'%(fname.replace('./docs/java/en/',''),methodname)
        namespaceinherited = "%s.%s"%(namespace,name)

        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(methodname,namespaceinherited,url,description,synopsis,'','java','en')


    #finds all methoddetailinfo
    for meth in findmethoddetail.findall("%s<HR>"%(findtable.sub('',getsection(file,methdet,end)).replace('<A NAME="method_detail"><!-- --></A>',''))):
      try:
        methodname = r1.sub('',findh3.findall(meth)[0]).strip()
        methodurl = finda.findall(meth)[0]
        methodurl = methodurl.replace('<A NAME="','').replace('">','')
        url = 'http://download.oracle.com/javase/6/docs/%s#%s'%(fname.replace('./docs/java/en/',''),methodurl)
        synopsis = findopenclosetags.sub('',findpre.findall(meth)[0].replace('<PRE>','').replace('</PRE>','').replace("\r\n",'').strip())
        description = spaces.sub(' ',findopenclosetags.sub('',findddtop.findall(meth)[0].replace('<DD>','').replace('<P>','')))
        namespaceinherited = "%s.%s"%(namespace,name)

        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(methodname,namespaceinherited,url,description,synopsis,'','java','en')
      except:
        pass
