from bs4 import BeautifulSoup
import requests

session = requests.Session()

def gather_spark_scala_html_files():
	spark_scala_html = session.get('{}{}'.format(spark_scala_base_url, "index.html"))
	
	soup = BeautifulSoup(spark_scala_html.text, 'html.parser')
	links = soup.find_all('a')
	
	for link in links:
		
		href = link.get('href')
		class_name_elem = link.find("span", class_ = "tplLink")
		
		if not class_name_elem:
			continue
		
		class_name = class_name_elem.text
		
		if 'org/apache/spark' in href:
			with open("download/{}".format(href.replace('/','.')), 'wb') as outfile:
				outfile.write(session.get('{}{}'.format(spark_scala_base_url, href)).text.encode("utf-8"))

if __name__ == '__main__':
	spark_scala_base_url = open('data.url').read().strip()
	gather_spark_scala_html_files()