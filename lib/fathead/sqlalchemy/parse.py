from bs4 import BeautifulSoup as bs
import bs4,re

class DocsParser(object):
    
    def __init__(self):
        self.events_page = None
        self.internals_page = None
        self.exceptions_page = None
        

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


    def write_to_file(self,filename,statement):
        """
            Function to append a given statement to
            the given file
        """

        currFile = open(filename,'a')
        currFile.write(statement)
        currFile.close()


    def generate_pages(self,page_name):
        """
            Function to parse the source code 
            of the internals page and write the 
            required functions onto output.txt
        """

        if page_name=='internals':
       	    soup = self.internals_page
	elif page_name=='events':
	    soup = self.events_page
	else:
	    soup = self.exceptions_page
        title_chck={}

        # Find all 'dl' tags in the page
        dls = soup.find_all('dl')

        for k in dls:

            curr_dd = k.find('dd') # Get the first 'dd' tag in the current 'dl' tag
            
            # The required components of each element
            section = '<section class="prog__container">{}</section>'
            code = '<pre><code>{}</code></pre>'
            desc='<p>{}</p>'
            header = '<span class="prog__sub">{}</span>'
            code_done=0                    
            txt='' # The string of all HTML elements
	    p='' # The description so far

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
                    elif elem.name=='div' and 'seealso' in elem['class']:
                        break# Ignore the divs which have 'see also' content
                    else:
                        if elem.name is not None:
                            p+=elem.text
			    code_done=0
                        else:
                            p+=elem
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

            # Text to be written to output.txt is stored in the variable 'to_write'
         
            # Check if title already exists 
            title=cur_code.text
            if title in title_chck: # Check if title already exists
                title=''
                class_list = cur_dt['id'].split('.')
                for s in class_list:
                    if s!='sqlalchemy' and s!='orm':
                        title+=s+' '
                title=title[0:len(title)-1]
                to_write = title+'\t'
                title_chck[title]=1
            else:
                to_write = cur_code.text+'\t';
                title_chck[title]=1

            to_write += 'A\t'
            to_write += '\t'*9
            to_write += section+'\t'
            to_write += 'http://docs.sqlalchemy.org/en/latest/orm/'+page_name+'.html#'+cur_dt['id']
            to_write += '\n'

	    # Append to the file
            self.write_to_file('output.txt',to_write)
  


if __name__=="__main__":
    dp = DocsParser()
    dp.get_pages()
    dp.generate_pages('internals')
    dp.generate_pages('events')
    dp.generate_pages('exceptions')
