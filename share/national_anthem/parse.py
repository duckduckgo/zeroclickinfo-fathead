from bs4 import BeautifulSoup
import codecs
import locale
import sys

filepath = "download/data.html"
with open(filepath, 'r') as f:
	data = f.read()

soup = BeautifulSoup(data)
table = soup.find('table')

sys.stdout = codecs.getwriter(locale.getpreferredencoding())(sys.stdout) 


count = 0
for row in table.findAll('tr'):
	if(count==0):
		pass
	else:
		print "national anthem","("+row.findAll('th')[0].span.string[:-2]+")","A",row.findAll('td')[0].a.string
	count = count+1