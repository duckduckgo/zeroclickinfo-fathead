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

files.append('./docs/backbonejs.html')


for file in files:
	filecontents = open(file).read()
	soup = BeautifulSoup(filecontents)
	for s in soup.findAll('p'):
		
		name = ''
		synopsis = ''
		
		try:
			name = openclosetags.sub('',str(s.findAll('b')[0]))
			#synopsis = openclosetags.sub('',str(s.findAll('code')[0]))
			synopsis = openclosetags.sub('', str(s.findNextSiblings('pre')[0])).replace("'","''")
			desc = openclosetags.sub('',spaces.sub(' ',badtags.sub('',str(s)))).replace("'","")
			url = "http://documentcloud.github.com/underscore/#%s"%(name)
			
			if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
				print "%s\t%s\t%s\t%s\t%s"%(name,'',url,desc,synopsis)
			if sys.argv[1].lower() == 'sql':
				print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,'','backbone.js','en')
			
		except:
			pass