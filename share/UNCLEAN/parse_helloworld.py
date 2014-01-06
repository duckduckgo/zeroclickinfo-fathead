from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)
badtags = re.compile('''<p.*>|</p>|<b.*>.*</b>|<code.*>.*</code>|<span.*>.*</span>|<.*?>|</.*?>''')
spaces = re.compile('''\s+''',re.DOTALL)

files = []

files.append('./docs/helloworld.html')


for file in files:
	filecontents = open(file).read()
	soup = BeautifulSoup(filecontents)
	for s in soup.findAll('a'):
		
		if 'table' in str(s):
		
			name = openclosetags.sub('',str(s.findAll('h2')[0])).replace("'","''")
			synopsis = ''
			description = ''
			
			try:
				synopsis = openclosetags.sub('',str(s.findNextSiblings('pre')[0])).replace("'","''")
			except:
				try:
					description = str(s.findNextSiblings('p')[0]).replace("'","''")
				except:
					if name == 'Piet':
						description = '<img src="http://helloworldsite.he.funpic.de/hellopics/piet.png">'
				
			url = "http://helloworldsite.he.funpic.de/hello.htm#%s"%(name)
			
			if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
				print "%s\t%s\t%s\t%s\t%s"%(name,'',url,description,synopsis)
			if sys.argv[1].lower() == 'sql':
				print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,description,synopsis,'hello world helloworld hw','helloworld','en')
			
