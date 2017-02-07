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

def extract_article_text(article, inline=False):
	article_text = ""

	for i in article.children:
		if i.name == "pre" or (i.name == "code" and not inline):
			article_text += "<pre><code>" + extract_article_text(i) + "</code></pre>"
		elif i.name == "code" and inline:
			article_text += "<code>" + i.text + "</code>"
		elif i.name == "ul":
			article_text += "<ul class='prog_ul'>" + extract_article_text(i) + "</ul>"
   		elif i.name == "li":
   			article_text += "<li>" + extract_article_text(i) + "</li>"
   		elif i.name =="p":
   			article_text += "<p>" + extract_article_text(i,True) + "</p>"
   		elif i.name != None:
   			article_text += i.text
   		else:
   			article_text += i
   	return article_text

def get_docs(filename):
	if filename[-5:] == ".html":
		soup = BeautifulSoup(open(filename), 'html.parser')
		title_link = soup.find_all("h1")[0]
		title_text = title_link.text
		
		url = BASE_URL + filename.replace("./download/api/", "") #title_link.find("a").get('href')

		article = soup.find_all("div", attrs={"class" : "comment"})[0]
		article_text = extract_article_text(article)


		return title_text, url, article_text

def get_tuple(filename):
	title , url ,abstract = get_docs(filename)
	data = ['']*13
	data[0] = title
	data[1] = 'A'
	data[12] = url
	abstract = abstract.replace("\n", "\\n").replace("\t","\\t")
	abstract = '<section class="prog_container">' + abstract + '</section>'
	data[11] = abstract
	return data

def output(filename="output.txt"):
	doc_files = get_all_files()
	line = ''
	i = 0
	for doc_file in doc_files:
		data = get_tuple( doc_file )
		line += "\t".join(data) + "\n"
		i+=1
		if i == 100:
			break
	f = open(filename, 'w')
	f.write(line.encode('utf'))
	f.close()

output()