import requests
from bs4 import BeautifulSoup

methodfile = open('./cover/methods.txt','w')
functionfile = open('./cover/functions.txt','w')

soup = BeautifulSoup(requests.get('http://docs.sqlalchemy.org/en/latest/orm/query.html', 'html5lib').text)

for method in soup.findAll('dl', attrs = {'class': 'method'}):
	for code in method.find('code', {'class': 'descname'}):
		methodfile.write(code + '\n')

methodfile.close()

for function in soup.findAll('dl', attrs = {'class': 'function'}):
	for code in function.find('code', {'class': 'descname'}):
		functionfile.write(code + '\n')

functionfile.close()

