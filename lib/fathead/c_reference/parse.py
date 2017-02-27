from bs4 import BeautifulSoup as bs, Comment, Doctype
import os
import string
import re

def main():
	filepath=[]
	filenames=[]
	wwwpath=[]
	for root, dirs, files in os.walk("download/en.cppreference.com/w/c"):
	    for file in files:
	    	if(".DS_Store" not in file):
	    		filenames.append(file)
	    		fp = os.path.join(root,file)
	    		filepath.append(fp)
	    		wwwpath.append(fp.replace("download/","http://"))

	for p in filepath:
		print p
		process_file(p)
#------------------------------------------------------------
def rm_tags( soup, vars ):
	for str in vars:
		[item.extract() for item in soup.find_all(str)]
#------------------------------------------------------------
def rm_comments(soup):
	comments = soup.find_all(text=lambda text:isinstance(text, Comment))
	[comment.extract() for comment in comments]
	
	doctype = soup.find_all(text=lambda text:isinstance(text, Doctype))
	[dc.extract() for dc in doctype]
#------------------------------------------------------------
def rm_args(soup, var1, var2):
	for var in var2:
		items = soup.find_all(attrs={var1:var})
		[item.extract() for item in items]
#------------------------------------------------------------
def process_links(text):
	processed_text=text.replace("href=\"/w/","href=\"http://en.cppreference.com/w/")
	return processed_text
#------------------------------------------------------------
def create_abstract_from_page(soup):
	soup.html.attrs={}
	soup.html.unwrap()
	soup.body.attrs={}
	soup.body.unwrap()
	soup.head.extract()
#------------------------------------------------------------
#------------------------------------------------------------
def process_span_nd_pre(body):
	spans = body.find_all("span")
	for s in spans:
		if not isinstance(s,basestring):
			if s.has_attr('class'):
				if s['class'][0]=="mw-geshi":
					[item.unwrap() for item in s.find_all("span")]
					s.name="code"
					s.attrs={}

	pres = body.find_all("pre")
	for s in pres:
		if not isinstance(s,basestring):
			[item.unwrap() for item in s.find_all("span")]
			s.name="pre"
			s.attrs=[]

	[item.unwrap() for item in body.find_all("span")]
	for item in body.find_all("div"):
		item.attrs=[]
#------------------------------------------------------------
def clean_rest_of_the_page(body):
	tags = body.find_all("",attrs={"id":re.compile("See_[aA]lso|References|Notes")})
	if(len(list(tags))>0):
		for tag in tags:
			while(tag.parent.next_sibling is not None):
				tag.parent.next_sibling.extract()
			tag.parent.extract()
			tag.extract()
#------------------------------------------------------------
def process_all_text_elements_for_newlines(body):
	for tag in body.find_all(re.compile('p|code')):
		if(tag.string is not None):
			sst=tag.string
			sst=re.sub(r'\n\n*','<br>',sst)
			tag.string=sst
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
def process_code_str(stri):
	stri=stri.replace("<pre>","<pre><code>")
	stri=stri.replace("</pre>","</code></pre>")
	return stri
#------------------------------------------------------------
def remove_unnecessary_divs(soup, tag):
	_tag = soup.find_all('div',{'id':tag})
	for t in _tag:
		# print "remming",t
		t.unwrap()
#------------------------------------------------------------
def rem_attr(soup, tag):
	_tag = soup.find_all(tag)
	for t in _tag:
		t.attrs = {}
#------------------------------------------------------------
def process_abstract(soup):
	#BS functions
	tags = ["table","tr","td"]
	[rem_attr(soup,t) for t in tags]

	unnecessary_divs = ["cpp-content-base","content","mw-content-text","bodyContent"]
	[remove_unnecessary_divs(soup,t) for t in unnecessary_divs]
		

	clean_rest_of_the_page(soup)
	process_span_nd_pre(soup)
	process_all_text_elements_for_newlines(soup)
	return_bare_abstract(soup)

	
	prefixed_string="<section class=\"prog__container\">"+str(soup)+"</section>"
	
	#String functions
	processed_body_text=process_links(prefixed_string)
	processed_body_text=process_code_str(processed_body_text)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("<div></div>","")\
	.replace("<p></p>","")
	processed_body_text=re.sub(r'\n\n*','', processed_body_text)
	processed_body_text=re.sub(r'>\n\n*<','><', processed_body_text)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("\n", "\\n")\
	.replace("\t", "    ")\
	.replace("&lt;br&gt;","<br>")
	return processed_body_text.decode('unicode-escape')
#------------------------------------------------------------
def cleanse_newlines(soup):
       qnt=1
       while(qnt>0):
               qnt=0
               for f in soup.children:
                       if f.name is None:
                               qnt=qnt+1
                               f.extract()
#------------------------------------------------------------
def return_bare_abstract(soup):
	cleanse_newlines(soup)

	for ch in soup.children:
		# print repr(ch)
		pass

	if len(list(soup.contents)) ==1:
		# print 'In'
		for f in soup.contents:
			# print repr(f)
			if f.name=='div' and f.parent==soup:
				# print "found",f.name
				f.unwrap()

	h3s=soup.find_all('h3')
	for ele in h3s:
		ele.name="span"
		ele.attrs={"class":"prog__sub"}
#------------------------------------------------------------
#------------------------------------------------------------
def process_file(filepath):
	file = open(filepath,'r')
	soup = bs(file, 'html.parser')
	filepath_elements = filepath.split("/")
	filename = filepath_elements[-1]
	
	_1_title = soup.title.string[:-len(" - cppreference.com")]
	_2_type_of_entry = 'A'
	_3_redirects_only = ""
	_4_leave_empty = ""
	_5_categories 	= ""#filepath_elements[-2]
	_6_leave_empty = ""
	_7_related_topics = ""
	_8_leave_empty = ""
	_9_extern_link = ""
	_10_disamb_pages = ""
	_11_image_link = ""
	_12_abstract = ""
	_13_src_url = filepath.replace("download/","http://")
	

	''' #TO BE USED IN A REDIRECTS.TXT FILE
	if(" " in _1_title and ", " not in _1_title):
		_3_redirects_only = filename
	else:
		_3_redirects_only = string.replace(_1_title,","," ").lower()
		if(filename not in [_3_redirects_only]):
			_3_redirects_only = _3_redirects_only+" "+filename
	'''

	rm_comments(soup)
	rm_tags(soup,['style','img','script','meta','link'])
	rm_args(soup,"id",["siteSub","contentSub","cpp-footer-base","mw-js-message","top","firstHeading"])
	rm_args(soup,"class",["noprint","t-nv","editsection","toctext","toc","tocnumber","t-navbar","t-navbar-head","t-navbar-menu","printfooter"])

	create_abstract_from_page(soup)
	try:
		_12_abstract=process_abstract(soup).encode('unicode-escape')

		# with open("./output.html",'w') as f:
			# f.write(_12_abstract)

		f_values = [_1_title, 
					_2_type_of_entry, 
					_3_redirects_only, 
					_4_leave_empty,  
					_5_categories, 
					_6_leave_empty, 
					_7_related_topics, 
					_8_leave_empty, 
					_9_extern_link, 
					_10_disamb_pages, 
					_11_image_link, 
					_12_abstract, 
					_13_src_url ]

		with open("./output.txt",'a') as f:
			f.write('\t'.join(f_values)+'\n')
	except Exception:
		print "Error in parsing"
#------------------------------------------------------------
if __name__ == "__main__":
	main()
