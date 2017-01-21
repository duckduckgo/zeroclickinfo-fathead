from bs4 import BeautifulSoup
import os

BASE_DIRECTORY = "./download/api/scala"
BASE_URL = "https://www.scala-lang.org/api/current/"


def get_all_files():
	doc_files = []

	for (path, dirs, files) in os.walk(BASE_DIRECTORY):
		for f in files:
			doc_files.append( "%s/%s" % (path, f))
	return doc_files

def get_docs(filename):
	if filename[-5:] == ".html":
		soup = BeautifulSoup(open(filename), 'html.parser')
		title_link = soup.find_all("h1")[0]
		title_text = title_link.text
		
		url = BASE_URL + filename.replace("./download/api/", "") #title_link.find("a").get('href')

		article_text = soup.find_all("div", attrs={"class" : "comment"})[0].text
		
		return title_text, url, article_text

def get_tuple(filename):
	title , url ,abstract = get_docs(filename)
	data = ['']*13
	data[0] = title
	data[1] = 'A'
	data[12] = url
	if abstract == None:
		abstract = "No data found"
	abstract = abstract.replace("\n", "\\n").replace("\t","\\t")
	abstract = '<section class="prog_container">' + abstract + '</section>'
	data[11] = abstract
	return data

def output(filename="output.txt"):
	doc_files = get_all_files()
	line = ''
	for doc_file in doc_files:
		data = get_tuple( doc_file )
		line += "\t".join(data) + "\n"

	f = open(filename, 'a')
	f.write(line.encode('utf'))
	f.close()

output()