import os
import re
from BeautifulSoup import BeautifulSoup
import sys
import string


def findindex(haystack, needle):
  count = 0
  for line in haystack:
    if needle in line:
      return count
    count += 1


def getsection(fd, start, end):
  html = ''
  for i in fd[start:end]:
    html = "%s\r\n%s" % (html, i)
  return html


def getall(fd):
  html = ''
  for i in fd:
    html = "%s\r\n%s" % (html, i)
  return html


r1 = re.compile(r'<.*?>', re.DOTALL)
findtr = re.compile(r'<TR .*?>.*?</TR>', re.DOTALL)
findtd = re.compile(r'<TD>.*?</TD>', re.DOTALL)
findtable = re.compile(r'<TABLE .*?</TABLE>', re.DOTALL)
findp = re.compile(r'<P>.*?<P>', re.DOTALL)
findpre = re.compile(r'<PRE>.*?</PRE>', re.DOTALL)
findh2 = re.compile(r'<H2>.*?</H2>', re.DOTALL)
findh3 = re.compile(r'<H3>.*?</H3>', re.DOTALL)
findcode = re.compile(r'<code>.*?</code>', re.DOTALL)
findcodeupper = re.compile(r'<CODE>.*?</CODE>', re.DOTALL)
findmethoddetail = re.compile(r'<A NAME.*?<HR>', re.DOTALL)
finda = re.compile(r'<A NAME.*?>', re.DOTALL)
findb = re.compile(r'<B>.*?</B>', re.DOTALL)
findddtop = re.compile(r'<DD.*?<P>', re.DOTALL)
findinherit = re.compile(r'<B>Methods inherited from.*?</TABLE>', re.DOTALL)
findopenclosetags = re.compile(r'<.*?>|</.*?>', re.DOTALL)
spaces = re.compile(r'\s+', re.DOTALL)

# java javax and org

#get all the files here
dirList = []


dir = "./docs/api/java/"

for (path, dirs, files) in os.walk(dir):
  if 'class-use' not in path:
    for f in files:
      dirList.append("%s/%s" % (path, f))

dir = "./docs/api/javax/"

for (path, dirs, files) in os.walk(dir):
  if 'class-use' not in path:
    for f in files:
      dirList.append("%s/%s" % (path, f))


first = True

for fname in dirList:
  fd = []

  #if fname == 'XmlAnyElement.html':
  #if fname  == 'RandomAccess.html': # interface
  if fname.endswith('.html') and 'package-' not in fname \
        and 'doc-files' not in fname:
    for line in open("%s" % fname):
      line = line.strip().replace("'", '')
      line = ''.join(filter(lambda x: x in string.printable, line))
      fd.append(line)

    start = findindex(fd, "START OF CLASS DATA")
    consum = findindex(fd, "CONSTRUCTOR SUMMARY")
    methsum = findindex(fd, "METHOD SUMMARY")
    condet = findindex(fd, "CONSTRUCTOR DETAIL")
    methdet = findindex(fd, "METHOD DETAIL")
    end = findindex(fd, "END OF CLASS DATA")

    #finds the name and namespace
    np = findh2.findall(getall(fd))[0]
    np = np.split('<BR>')
    namespace = r1.sub('', np[0]).strip()
    classtype = r1.sub('', np[1]).strip()

    #if its an interface skip it
    if 'interface' in classtype.lower():
      continue

    #finds the description which is the large text at the beginning
    desc = findp.findall(getall(fd))[0]

    # print the object

    name = fname.split('/')[-1].replace('.html', '')
    url = "http://download.oracle.com/javase/8/docs/%s" \
        % (fname.replace('./docs/java/en/', ''))
    description = spaces.sub(' ', findopenclosetags.sub('', desc).strip())

    print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" % (name, namespace, url,
                                              description, '', '',
                                              'java', 'en')

    #finds all inherited methods
    for i in findinherit.findall(getall(fd)):
      description = spaces.sub(' ',
                               findopenclosetags.sub('', 
                                                     findb.findall(i)[0].replace('Methods','Method').replace('<B>','').replace('</B>','')
                                                     )
                               )
      #print detail
      for j in findcodeupper.findall(i)[0].replace('<CODE>', '').replace('</CODE>', '').split('>, '):
        #synopsis = j.strip().replace('</A','</A>').replace('>>','>')
        synopsis = ''
        methodname =  r1.sub('', j).replace('</A', '').strip()
        url = 'http://download.oracle.com/javase/8/docs/%s#%s' % ( fname.replace('./docs/java/en/',''), methodname)
        namespaceinherited = "%s.%s" % (namespace, name)

        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" % (methodname,
                                                  namespaceinherited, url,
                                                  description, synopsis, '', 
                                                  'java', 'en')


    #finds all methoddetailinfo
    for meth in findmethoddetail.findall("%s<HR>" % (findtable.sub('', getsection(fd, methdet, end)).replace('<A NAME="method_detail"><!-- --></A>', ''))):
      try:
        methodname = r1.sub('', findh3.findall(meth)[0]).strip()
        methodurl = finda.findall(meth)[0]
        methodurl = methodurl.replace('<A NAME="', '').replace('">', '')
        url = 'http://download.oracle.com/javase/8/docs/%s#%s'%(fname.replace('./docs/java/en/', ''),methodurl)
        synopsis = findopenclosetags.sub('',findpre.findall(meth)[0].replace('<PRE>', '').replace('</PRE>', '').replace("\r\n", '').strip())
        description = spaces.sub(' ',findopenclosetags.sub('', findddtop.findall(meth)[0].replace('<DD>', '').replace('<P>', '')))
        namespaceinherited = "%s.%s" % (namespace, name)

        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(methodname, namespaceinherited,
                                                url, description, synopsis,
                                                '','java','en')
      except:
        pass
