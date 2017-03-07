from bs4 import BeautifulSoup
import codecs
import sys

filepath = "download/data.html"
with open(filepath, 'r') as f:
	data = f.read()

soup = BeautifulSoup(data)
table = soup.find('table')

#For Unicode Output Format
sys.stdout = codecs.getwriter("UTF-8")(sys.stdout) 


count = 0
for row in table.findAll('tr'):
	if(count==0):
		count = count +1
	else:
		if row.findAll('td')[4].a is not None:
			print "national anthem\t","("+row.findAll('th')[0].span.string[:-2]+")\t","A\t",row.findAll('td')[0].a.string,"\t"+"<audio controls><source src='https:"+row.findAll('td')[4].a['href']+"'type='audio/ogg'></audio>"