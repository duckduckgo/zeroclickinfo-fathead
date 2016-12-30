from bs4 import BeautifulSoup
import glob

spark_scala_base_url = open('data.url').read().strip()

class Description(object):
	def __init__(self, name, annotation, description, source_url):
		self.name = name
		self.description = '<p>{}</p>'.format(description)
		self.annotation = annotation
		self.source_url = source_url
	
	def get_description(self):
		if self.annotation:
			self.description = '<p>{}</p>{}'.format(self.annotation, self.description)
		self.description = '<section class="prog__container">{}</section>'.format(self.description)
		return '\t'.join([
            self.name,  # Full article title
            'A',  # Type of article
            '',  # For redirects only
            '',  # Ignore
            '',  # Categories
            '',  # Ignore
            '',  # Related Topis
            '',  # Ignore
            '',  # External links
            '',  # For disambiguation pages only
            '',  # Image
			self.description, # Abstract
			self.source_url
		])
	
class Parser(object):
	
	def __init__(self):
		'''Get all the spark scala api class file need to be parsed'''
		self.files_to_parse = glob.glob('download/*.html')
		
	def parse_comment(self):
		'''Parse each of spark scala apit class file and make a Description object for each'''
		#self.descriptions = []
		with open('output.txt', 'wb') as output:
			for file in self.files_to_parse:
				soup = BeautifulSoup(open(file), 'html.parser')
				name = file.split("/")[1].replace('.html', '')

				source_url = '{}{}.html'.format(spark_scala_base_url, name.replace('.', '/'))
				comment_element = soup.find('div', id = 'comment').find(class_ = 'comment cmt')

				if not comment_element:
					continue

				description_with_annotation = u' '.join([p.text for p in comment_element.find_all("p")])

				description_with_annotation_list = description_with_annotation.split(u'::')
				annotation = None

				#Either has an annotation or not
				assert len(description_with_annotation_list) == 3 or len(description_with_annotation_list) == 1, name
				
				
				#Has annotation
				if len(description_with_annotation_list) == 3:
					annotation = unicode.strip(description_with_annotation_list[1])
					description = unicode.strip(description_with_annotation_list[2])
				#Doesn't have annotation
				elif len(description_with_annotation_list) == 1:
					description = unicode.strip(description_with_annotation_list[0])

				description = u' '.join(unicode.split(description))

				description_object = Description(name, annotation, description, source_url)
				output.write((description_object.get_description() + '\n').encode('utf-8'))

if __name__ == '__main__':
	parser = Parser()
	parser.parse_comment()