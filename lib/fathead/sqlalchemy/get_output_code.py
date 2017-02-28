from bs4 import BeautifulSoup

soup = BeautifulSoup(open("output.txt"))


methodfile = open("./cover/methods.txt", "wb")

for section in soup.find_all('section', attrs = {'class': 'prog__container'}):
	for code in section.find('code'):
		#print(code)
		methodfile.write(code.encode('utf-8') +"\n")

methodfile.close()
