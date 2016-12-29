from bs4 import BeautifulSoup as bs
import bs4,re

class DocsParser(object):
    
    def __init__(self):
        self.events_page = None
        self.internals_page = None
        self.exceptions_page = None
        self.elements = {}
	self.repeated = {} 
	self.description = {}
	self.titles = {}

    def get_pages(self):
        """
            Function to get the source code of the tutorials page 
        """

        file_loc = 'download/events.html'
        file = open(file_loc,'r+')

        code_str = '' # Get the entire source code as a string
        
        for k in file.readlines():
            code_str+=k

        self.events_page = bs(code_str,'html.parser')

        file_loc = 'download/internals.html'
        file = open(file_loc,'r+')

        code_str = '' # Get the entire source code as a string
        
        for k in file.readlines():
            code_str+=k

        self.internals_page = bs(code_str,'html.parser')

        file_loc = 'download/exceptions.html'
        file = open(file_loc,'r+')

        code_str = '' # Get the entire source code as a string
        
        for k in file.readlines():
            code_str+=k

        self.exceptions_page = bs(code_str,'html.parser')


    def replace_unicodes(self,txt):
	"""
	   Function to replace all unicodes with their HTML
	    equivalent
	"""

        txt=txt.replace('\n','\\n')
        txt=txt.replace('\t','\\t')
        txt=txt.replace(u'\u2019','&#8217;')
        txt=txt.replace(u'\u201c','&#8220;')
        txt=txt.replace(u'\u201d','&#8221;')
        txt=txt.replace(u'\xb6','')
        txt=txt.replace(u'\u2013','&#8211;')
        txt=txt.replace(u'\u2018','&#8216;')

        return txt


    def write_to_file(self,filename,elements):
        """
            Function to append a given statement to
            the given file
        """
	# Append to the file
	currFile = open(filename,'a')

	for e in elements:
            currFile.write(elements[e])

        currFile.close()

    def remove_newline(self,text):
	"""
	    Function to remove '\n'
	    from the start of a sentence
	"""
	t_list = text.split('\\n')
	txt = ''
	
	for k in t_list:
	    if k!='' and k!=' ':
	        txt += k+' '

	return txt

    def get_table_contents(self,table):
	"""
	    Function to return the 
	    contents of a table after
	    checking for code snippets
	"""

	if table.find('div',{'class':'highlight-default'}) is None:
	    return table.text # If table doesn't have snippets then return entire text
	
	tbody = table.find('tbody')

	th =  tbody.find('th')
	text = '\\n\\n'+th.text
	td = tbody.find('td')
	ul = td.find('ul')
	text += '<ul>{}</ul>' # Fetch the elements and insert them into unordered list
	lis=''

	for li in ul:
	    cur = '<li>{}</li>\\n'
	    txt=''
	    for e in li:
		try:
		    if e.name=='div' and 'highlight-default' in e['class']:
			txt+='<pre><code>{}</code></pre>'.format(e.text)
		    else:
			txt+=e.text
		except:
		    if str(type(e))=="<class 'bs4.element.NavigableString'>":
			txt+=e.string

	    cur=cur.format(self.replace_unicodes(txt))
	    lis+=cur
	text = text.format(lis)

	return text
		

	
    def generate_pages(self,page_name):
        """
            Function to parse the source code 
            of the internals page and write the 
            required functions onto output.txt
        """
	
	elements = self.elements
	repeated = self.repeated
	description = self.description
	title_chck = self.titles

        if page_name=='internals':
       	    soup = self.internals_page
	elif page_name=='events':
	    soup = self.events_page
	else:
	    soup = self.exceptions_page

        # Find all 'dl' tags in the page
        dls = soup.find_all('dl')

        for k in dls:

            curr_dd = k.find('dd') # Get the first 'dd' tag in the current 'dl' tag
            
            # The required components of each element
            section = '<section class="prog__container">{}</section>'
            code = '<pre><code>{}</code></pre>'
            desc='<p>{}</p>'
            header = '<span class="prog__sub" style="font-weight:bolder;">{}</span>'
            code_done=0                    
            txt='' # The string of all HTML elements
	    p='' # The description so far
	    first_p = '' # The first paragraph
	    is_first_p = 1

            # Loop through the 'dd' tag in the current 'dl' tag 
            for elem in curr_dd:
                if elem is None or str(type(elem))=="<class 'bs4.element.NavigableString'>":# Skip the 'None' elements and non-tags
                    continue
                if elem.name=='dl':# Break if you come across a new 'dl' tag inside the current
                    break
                else:
                    if elem.name=='div' and ('highlight-default' in elem['class'] or ('event-signatures' in elem['class'])) :
                        strg=''
			if 'event-signatures' in elem['class']:# Add event signatures separately
			    ptag = elem.find('p')
			    p+=ptag.text
			    divtag= elem.find('div',{'class':'highlight-default'})
			    strg+=divtag.text
			else:
			    strg += elem.text
			p = self.replace_unicodes(p)# Replace all unicodes with HTML equivalent
                        strg = self.replace_unicodes(strg)# Replace all unicodes with HTML equivalent
			txt += desc.format(p)+code.format(strg)
			p=''
                        code_done=1
			is_first_p=0
                    elif elem.name=='div' and 'seealso' in elem['class']:
                        break# Ignore the divs which have 'see also' content
		    elif elem.name=='table':
			p+=self.get_table_contents(elem)
			p = self.replace_unicodes(p)
                    else:
                        if elem.name is not None:
                            p+=elem.text
			    if is_first_p:
				first_p+=elem.text
				is_first_p=0
			    code_done=0
                        else:
                            p+=elem
			    if is_first_p:
				first_p+=elem
				is_first_p=0
			    code_done=0


            cur_dt = k.find('dt')
            
            # Initialize and find the value of the current main topic
            head = ''
            for d in cur_dt:
                if d is None or str(type(d))=="<class 'bs4.element.NavigableString'>":
                    head+=d.string
                    continue
                if d.name=='a' and d['class'][0]=='headerlink':
                    break
                head+=d.string
            
            # Replace unicodes with HTML equivalent in the head
            head= head.replace(u'\u2192','&#8594;')
            head = head.replace('\n','\\n')
            head= head.replace('\t','\\t')

            header=header.format(head)
             
            
            if code_done==1:
                section = section.format(header+txt)
            else:
		p = self.replace_unicodes(p)
		txt+=desc.format(p)
                section = section.format(header+txt)
	

            # 'descname' is the current redirect name 
            cur_code = cur_dt.find('code',{'class':'descname'})
         
            # Get the full title of the element along with its class name
            title=''
	    write=0
            class_list = cur_dt['id'].split('.')
            for s in class_list:
                if s!='sqlalchemy' and s!='orm':
                    title+=s+'.'
            title=title[0:len(title)-1]

	    if cur_code.text in title_chck:
                to_write = title+'\t'
                if cur_code.text in repeated:
                    repeated[cur_code.text].append(title)
                else:
                    repeated[cur_code.text]=[title]
		write=1
            else:
                to_write = cur_code.text+'\t';
                title_chck[cur_code.text]=title

	    # Elements to be written to file
            to_write += 'A\t'
            to_write += '\t'*9
            to_write += section+'\t'
            to_write += 'http://docs.sqlalchemy.org/en/latest/orm/'+page_name+'.html#'+cur_dt['id']
            to_write += '\n'

	    if write:
	        elements[title] = to_write
		description[title] = self.replace_unicodes(first_p)
		description[title] = self.remove_newline(description[title])
	    else:
		elements[cur_code.text] = to_write
		description[cur_code.text] = self.replace_unicodes(first_p)
		description[cur_code.text] = self.remove_newline(description[cur_code.text])

    
    def disambiguate(self):

        elements = self.elements
        repeated = self.repeated
        description = self.description
        title_chck = self.titles

	# Add repeated elements as disambiguation entries
        for k in repeated:
	    # Go through all elements with repeated names and add them to the dismabiguation entry

	    full_title = title_chck[k]
	    cur = ''
	    pos = len(full_title)-1
	    cur_val = elements[k]

	    while full_title[pos]!='.' and pos>=0:
		cur+=full_title[pos]
		pos=pos-1

	    cur = cur[::-1]

	    elements[title_chck[k]] = full_title+cur_val[len(cur):len(cur_val)]
	    elements[k] = ''

	    title = "*[[{}]],"

	    to_write = k+'\t'
	    to_write += 'D\t'
	    to_write += '\t'*7
	    to_write += title.format(title_chck[k])+' '+description[k]+'\\n'
	
	    for z in repeated[k]:
		to_write += title.format(z)+' '+description[z]+'\\n'
	    to_write +='\n'
	    elements[k] = to_write
	
	# Write to file after adding disambiguation entries
	self.write_to_file('output.txt',elements)	    


if __name__=="__main__":
    dp = DocsParser()
    dp.get_pages()
    dp.generate_pages('internals')
    dp.generate_pages('events')
    dp.generate_pages('exceptions')
    dp.disambiguate() # Add the disambiguation entries and write to file
