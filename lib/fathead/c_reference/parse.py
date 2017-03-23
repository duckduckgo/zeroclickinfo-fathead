from bs4 import BeautifulSoup as bs, Comment, Doctype
import os
import string
import re

def main():

	filepath=[]
	for root, dirs, files in os.walk("download/c"):
	    for file in files:
	    	if(".DS_Store" not in file):
		    	fp = os.path.join(root,file)
		    	filepath.append(fp)
	filescount=0
	for p in filepath:
		# p="download/c/numeric/math/asin.html"
		filescount=filescount+1
		print filescount, p
		process_file(p)
		# raw_input()

	post_processing()
#------------------------------------------------------------
def post_processing():
	print "Post Processing\n"
	faulty_pages=["http://en.cppreference.com/w/c/numeric/complex.html",
	"http://en.cppreference.com/w/c/language/static_assert.html"]

	with open('./cover/keywords.txt') as k:
		keyws = k.read().splitlines()
	with open('./cover/functions.txt') as k:
		funcs = k.read().splitlines()
	with open('./output_keywords.txt') as o_key:
		output_keywords = o_key.read().splitlines()
	with open('./output_rest.txt') as o_rest:
		output_rest = o_rest.read().splitlines()

	for i in xrange(len(keyws)):
		if keyws[i] not in funcs:
			write_to_output_post_process("./output.txt",output_keywords[i])
	
	for fs in output_rest:
		if fs.split("\t")[-1] not in faulty_pages:
			write_to_output_post_process("./output.txt", fs)

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
 		l.unwrap()
#------------------------------------------------------------
def create_abstract_from_page(soup):
	soup.html.unwrap()
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
def process_divs(soup):
	tags=["t-li1","t-li2","t-li3"]
	for tag in tags:
		_tag = soup.find_all('div',tag)
		for t in _tag:
			t.attrs={}
			t.name="ul"

	ff=soup.find_all("div","t-dsc-member-div")
	for gg in ff:
		gg.attrs={}
		gg.children.next().name="code"
	
	for tag in soup.find_all('div'):
		tag.unwrap()
#------------------------------------------------------------
def rem_attr(soup, tag):
	_tag = soup.find_all(tag)
	for t in _tag:
		t.attrs = {}
#------------------------------------------------------------
def cleanup_code_tag_for_ps(soup):
	ps=[]
	for _t in soup.find_all('code'):
		for _tc in _t.descendants:
			if _tc is not None:
				if not isinstance(_tc, basestring):
					if _tc.name=='p':
						ps.append(_tc)
	for pis in ps:
		pis.unwrap()

#------------------------------------------------------------
def process_abstract(soup):
	#BS functions
	tags = ["table","tr","td"]
	[rem_attr(soup,t) for t in tags]

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

	cleanup_code_tag_for_ps(soup)

	
	prefixed_string="<section class=\"prog__container\">" \
					+ str(soup) \
					+ "<style type=\"text/css\">.t-su {display: inline-block;margin-bottom: -0.3em;vertical-align: 0.8em;line-height: 1.2em;font-size: 85%;text-align: left}.t-su-b {vertical-align: -0.4em}"\
					+ ".t-mfrac > table {display: inline-block;vertical-align: middle;border-spacing: 0;}.t-mfrac > table > tbody > tr:first-child > td {border-bottom: 0.1em solid}"\
					+ "</style>" \
					+ "</section>"
	'''
	+ "<style type=\"text/css\">.t-su {display: inline-block;margin-bottom: -0.3em;vertical-align: 0.8em;line-height: 1.2em;font-size: 85%;text-align: left}.t-su-b {vertical-align: -0.4em}"\
	+ ".t-mfrac > table {display: inline-block;vertical-align: middle;border-spacing: 0;}.t-mfrac > table > tbody > tr:first-child > td {border-bottom: 0.1em solid}"\
	+ "</style>" \
	'''	
	#String functions
	processed_body_text=process_code_str(prefixed_string)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("<p></p>","")\
	.replace("</br>","<br>")
	processed_body_text=re.sub(r'\n\n*','', processed_body_text)
	processed_body_text=re.sub(r'>\n\n*<','><', processed_body_text)
	processed_body_text=processed_body_text.strip("\n")\
	.replace("\n", "\\n")\
	.replace("\t", "    ")\
	.replace("&lt;br&gt;","<br>")\
	.replace("&lt;","<")\
	.replace("&gt;",">")\
	.replace("&nbsp;"," ")
	processed_body_text=re.sub(r'<br>(<br>)*','<br>', processed_body_text)
	processed_body_text=process_escaped_chars(processed_body_text)
	return processed_body_text
#------------------------------------------------------------
def process_escaped_chars(text):
	escaped_chars = [r"\\0", r"\\1", r"\\x", r"\\t", r"\\n"]
	for echar in escaped_chars:
		rechar = r"\\"+ echar
		# print "pre_rechar :", re.search(rechar, text)
		text = re.sub(echar, rechar, text)
		# print re.search(echar, text)
		# print "pos_rechar :", re.search(rechar, text),
	return text
#------------------------------------------------------------
def return_bare_abstract(soup):

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

	ff=soup.find_all("span",{"t-lc","t-dcl-list-see-monospace","t-dsc-see-tt"})
	for gg in ff:
		gg.attrs={}
		gg.name="code"

	ff=soup.find_all("span",{"t-spar"})
	for gg in ff:
		gg.attrs={}
		gg.name="i"
#------------------------------------------------------------
def write_to_file(path, item):
	try:
		with open(path,'a') as file:
			file.write(item+"\n")
	except Exception as e2:
		print "Error in writing to", path
		print(str(e2))
#------------------------------------------------------------
#------------------------------------------------------------
def write_to_abstract(path, f_values):
	try:
		with open(path,'a') as f:
			i=0
			for i in xrange(len(f_values)):
				f.write(f_values[i])
				if(i!=12): f.write("\t")
			f.write("\n")
	except Exception as e1:
			print "Error in writing to ",path
			print(str(e1))
#------------------------------------------------------------
def write_to_output_post_process(path, f_values):
	try:
		with open(path,'a') as f:
			f.write(f_values+"\n")
	except Exception as e1:
			print "Error in writing to ",path
			print(str(e1))
#------------------------------------------------------------
def process_file(filepath):
	file = open(filepath,'r')
	soup = bs(file, 'html.parser')
	with open("./input.html","w") as g:
			g.write(str(soup))

	filepath_elements = filepath.split("/")
	# print filepath_elements

	docname = filepath_elements[-1][:-len(".html")]
	actual_page_title = soup.title.string[:-len(" - cppreference.com")]

	parent_topic = filepath_elements[-2]
	
	_1_title = docname #This will always be distinct
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
	

	#TO BE USED IN A REDIRECTS.TXT FILE
	if(" " in actual_page_title and ", " not in actual_page_title):
		to_write=docname+", "+actual_page_title.strip()
		write_to_file("./redirects.txt",to_write)
	elif "," in actual_page_title:
		for item in actual_page_title.split(","):
			if(item!=docname):
				to_write=docname+", "+item.strip()
				write_to_file("./redirects.txt",to_write)
	else:
		pass
		


	rm_comments(soup)
	rm_tags(soup,['style','img','script','meta','link'])
	rm_args(soup,"id",["siteSub","contentSub","cpp-footer-base","mw-js-message","top","firstHeading","Return_value_2"])
	rm_args(soup,"class",["noprint","t-nv","editsection","toctext","toc","tocnumber","t-navbar","t-plot","t-navbar-head","t-navbar-menu","printfooter","t-mark-rev"])

	process_links(soup)

	create_abstract_from_page(soup)
	convert_css_to_html(soup)
	process_divs(soup)

	try:
		_12_abstract=process_abstract(soup)
		with open("./output.html",'w') as f:
			f.write(_12_abstract)
		#print "Written to output.html"

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

		
		if(parent_topic=="keyword"):
			write_to_abstract("./output_keywords.txt", f_values)
			write_to_file("./cover/keywords.txt", docname)
		else:
			write_to_abstract("./output_rest.txt", f_values)
			write_to_file("./cover/functions.txt", docname)
		
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
