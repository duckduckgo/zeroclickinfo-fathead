from bs4 import BeautifulSoup as bs, Comment, Doctype
import os
import string
import re

def main():
	filepath=[]
	filenames=[]
	for root, dirs, files in os.walk("download/c"):
	    for file in files:
	    	if(".DS_Store" not in file):
		    	filenames.append(file)
		    	fp = os.path.join(root,file)
		    	filepath.append(fp)

	for p in filepath:
		# p="download/c/language/escape.html"
		print p
		process_file(p)
		# raw_input()
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
def process_links(soup):
	links = soup.find_all("a")
	for l in links:
		if l.has_attr("title"):
			rel_p=l['href']
			abs_p=os.path.abspath(l['href'])
			url=re.sub(r'((.*?)/fathead/)','http://en.cppreference.com/w/',abs_p)
			url=re.sub(r'(/c_reference/)','/c/',url)
			l['href']=url

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
			s.attrs={}


	acceptable_spans = ['t-su','t-su-b','t-mfrac']
	for sp in body.find_all("span"):
		if sp.has_attr('class') and sp['class'][0] not in acceptable_spans:
			sp.unwrap()

	for item in body.find_all("div"):
		item.attrs={}
#------------------------------------------------------------
def clean_rest_of_the_page(body):
	tags = body.find_all("",attrs={"id":re.compile("See_[aA]lso|References|Notes|Example")})
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
			sst=re.sub(r'\n(\n)*','<br>',sst)
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
	# print "rem_attr\n\n",soup
	# raw_input()

	unnecessary_divs = ["cpp-content-base","content","mw-content-text","bodyContent"]
	[remove_unnecessary_divs(soup,t) for t in unnecessary_divs]
	# print "unnecessary divs\n\n",soup
	# raw_input()
		

	clean_rest_of_the_page(soup)
	# print "rest of page\n\n",soup
	# raw_input()

	process_span_nd_pre(soup)
	# print "span and pre\n\n",soup
	# raw_input()

	process_all_text_elements_for_newlines(soup)
	# print "all text elements of new liwns\n\n",soup
	# raw_input()

	return_bare_abstract(soup)
	# print "return bare extract\n\n",soup
	# raw_input()

	
	prefixed_string="<section class=\"prog__container\">" \
					+ str(soup) \
					+ "<style type=\"text/css\">.t-su {display: inline-block;margin-bottom: -0.3em;vertical-align: 0.8em;line-height: 1.2em;font-size: 85%;text-align: left}.t-su-b {vertical-align: -0.4em}"\
					+ ".t-mfrac > table {display: inline-block;vertical-align: middle;padding: 0;border-spacing: 0;text-align: center;font-size: 0.9em}.t-mfrac > table > tbody > tr > td {border: none}.t-mfrac > table > tbody > tr:first-child > td {border-bottom: 0.1em solid}"\
					+ "</style>" \
					+ "</section>"
	
	#String functions
	processed_body_text=process_code_str(prefixed_string)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("<div></div>","")\
	.replace("<p></p>","")\
	.replace("</br>","<br>")
	processed_body_text=re.sub(r'<br>(<br>)*','<br>', processed_body_text)
	processed_body_text=re.sub(r'\n\n*','', processed_body_text)
	processed_body_text=re.sub(r'>\n\n*<','><', processed_body_text)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("\n", "\\n")\
	.replace("\t", "    ")\
	.replace("&lt;br&gt;","<br>")\
	.replace("&nbsp;"," ")
	return processed_body_text
#------------------------------------------------------------
#------------------------------------------------------------
def return_bare_abstract(soup):
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
		ele.wrap(soup.new_tag("h3"))
		ele.name="span"
		ele.attrs={"class":"prog__sub"}
#------------------------------------------------------------
def convert_css_to_html(soup):
	ff=soup.find_all("span","t-lines")
	for gg in ff:
		hh=gg.find_all("span")
		for ii in hh:
			if ii.attrs=={}:
				ii.name="td"
				ii.wrap(soup.new_tag("tr"))
		gg.name="table"
		gg.attrs={}

	ff=soup.find_all("div","t-dsc-member-div")
	for gg in ff:
		gg.attrs={}
		gg.children.next().name="code"

	ff=soup.find_all("span",{"t-lc","t-dcl-list-see-monospace","t-dsc-see-tt"})
	for gg in ff:
		gg.attrs={}
		gg.name="code"

	ff=soup.find_all("span",{"t-spar"})
	for gg in ff:
		gg.attrs={}
		gg.name="i"
#------------------------------------------------------------
def process_file(filepath):
	file = open(filepath,'r')
	soup = bs(file, 'html.parser')
	with open("./input.html","w") as g:
			g.write(str(soup))

	filepath_elements = filepath.split("/")
	filename = filepath_elements[-1][:-len(".html")]
	actual_page_title = soup.title.string[:-len(" - cppreference.com")]

	
	_1_title = filename #This will always be distinct
	_2_type_of_entry = 'A'
	_3_redirects_only = ""
	_4_leave_empty = ""
	_5_categories 	= ""
	_6_leave_empty = ""
	_7_related_topics = ""
	_8_leave_empty = ""
	_9_extern_link = ""
	_10_disamb_pages = ""
	_11_image_link = ""
	_12_abstract = ""
	_13_src_url = filepath.replace("download/","http://en.cppreference.com/w/")
	

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
	rm_args(soup,"id",["siteSub","contentSub","cpp-footer-base","mw-js-message","top","firstHeading","Return_value_2"])
	rm_args(soup,"class",["noprint","t-nv","editsection","toctext","toc","tocnumber","t-navbar","t-plot","t-navbar-head","t-navbar-menu","printfooter"])

	process_links(soup)

	create_abstract_from_page(soup)
	convert_css_to_html(soup)
	try:
		_12_abstract=process_abstract(soup)		
		with open("./output.html",'w') as f:
			f.write(_12_abstract)
		print "Written to output.html"

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
					
		try:
			with open("./output.txt",'a') as f:
				for fv in f_values:
					f.write(fv+"\t")
				f.write("\n")
		except Exception as e1:
			print "Error in writing to output.TXT"
			print(str(e1))

	except Exception as e:
		print "Error in Processing : "
		print(str(e))
		print "Abstract : " ,_12_abstract
		raw_input()
#------------------------------------------------------------
if __name__ == "__main__":
	main()


#____________________--
#".t-vertical {width: 1.5em;white-space: nowrap;margin: 0 auto}.t-vertical > div {-moz-transform: translate(50%, 50%) rotate(90deg);-moz-transform-origin: 50%\ 0%;-o-transform: translate(50%, 50%) rotate(90deg);-o-transform-origin: 50%\ 0%;-webkit-transform: translate(50%, 50%) rotate(90deg);-webkit-transform-origin: 50%\ 0%;-ms-transform: translate(50%, 50%) rotate(90deg);-ms-transform-origin: 50%\ 0%;transform: translate(50%, 50%) rotate(90deg);transform-origin: 50%\ 0%;float: right}"\
#____________________--
