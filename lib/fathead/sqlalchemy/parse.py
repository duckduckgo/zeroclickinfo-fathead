from bs4 import BeautifulSoup as bs
import bs4,re

class TutorialParser(object):
	
	def __init__(self):
		self.page = None

	def get_page(self):
		"""
			Function to get the source code of the tutorials page 
		"""

		file_loc = 'download/tutorial.html'
		file = open(file_loc,'r+')

		code_str = '' # Get the entire source code as a string
		
		for k in file.readlines():
			code_str+=k

		self.page = bs(code_str,'html.parser')


	def generate(self):
		"""
			Function to parse the source code 
			of the tutorials page and write the 
			required functions onto output.txt
		"""

		file = open('output.txt','w')

		soup = self.page

		parent_section =soup.find('div',{'class':'section'}) 
		sections = parent_section.find_all('div',{'class':'section'})

		# Main loop
		for section in sections:
			prog_container = '<section class="prog__container">{}</section>'
			description = '<p>{}</p>'
			code = '<pre><code>{}</code></pre>'
			
			title = section.find('h2')
			title1 = section.find('h3')
			if title is not None:
				title = title.text
			elif title1 is not None:
				title = title1.text
			else:
				continue

			txt=''

			for k in section:
				if k is not None and isinstance(k,bs4.element.Tag):
					if k.name=='p':
						txt+=k.text
					elif k.name=='div':
						if 'highlight-python' in k['class'][0]:
							break

			section_code = section.find('pre')

			if section_code is None:
				continue

			section_code = section_code.text
			section_code = section_code.encode('ascii','ignore')
			txt = txt.encode('ascii','ignore')

			code = code.format(section_code)
			if txt=='':
				description=''
			else:
				description=description.format(txt)
			description = description.replace('\n','\\n')
			code = code.replace('\n','\\n')
			code = code.replace('\\\\n','\\n')
			prog_container = prog_container.format(description+code)
			

			if title is not None:
				title = title.encode('ascii','ignore')
				title = title.lower()
				ls = list(title)
				ls[0] = ls[0].upper()
				title = ''.join(ls)
				file.write(title)

			file.write('	')
			file.write('A')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write('	')
			file.write(prog_container)
			file.write('	')
			file.write('http://docs.sqlalchemy.org/en/latest/orm/tutorial.html')
			file.write('\n')

		file.close()

	


if __name__=="__main__":
	tp = TutorialParser()
	tp.get_page()
	tp.generate()
