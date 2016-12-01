import os
from bs4 import BeautifulSoup
URL_ROOT = 'http://www.boost.org'
URL_DOC = ''


DOWNLOADED_HTML_PATH = 'download/'

class BoostData(object):
   
    def __init__(self):
        
        self.HTML = ""
        self.load_data()

    def load_data(self):
        
        with open('download/index.html','r') as data_file:
            self.HTML= data_file.read()

    def get_raw_data(self):
       
        return self.HTML



class BoostDataParser:


    def __init__(self,raw_data):

        #self.parsed_data=None
        
        self.soup_data=BeautifulSoup(raw_data,'html.parser')
        self.doc_content=self.soup_data.find('div',{'class':'section-body'})
##        for tag in soup.find_all('dl',{'class':'fields'}):
##            gop=tag.findAll('dt')
##            for gopa in gop:
##                gopa.replaceWith('')
##        tt_all=soup.find_all('dt')
##        for t in tt_all:
##            print(t.text,'\t',t.find_next('p').text,'\t','{}{}'.format(URL_ROOT,t.find_next('a').get('href')))
    
    def parse_name_and_anchor(self,section):

        name=section.text
        anchor='{}{}'.format(URL_ROOT,section.find_next('a').get('href'))
        return name, anchor

        

    def filter_data(self,section):

        for dl_elem in section.find_all('dl',{'class':'fields'}):
            dt=dl_elem.find_all('dt')
            for dt_elem in dt:
                dt_elem.replaceWith('')
        self.filtered_doc=section.find_all('dt')
        

    
    def parse_first_paragraph(self,section):
        
        return section.find_next('p').text.replace('\n','')


    def parse_final_data(self):

        data=[]

        for section in self.filtered_doc:
            name, anchor = self.parse_name_and_anchor(section)
            first_paragraph=self.parse_first_paragraph(section)

            data_elem = {
                'name' : name,
                'first_paragraph' : first_paragraph,
                'anchor' : anchor
                }
            data.append(data_elem)
        self.parsed_data = data

    def get_data(self):
        return self.parsed_data
            
        
        






if __name__=="__main__":
    
    boost_data=BoostData()
    raw_data=boost_data.get_raw_data()
    #print(raw_data)
    boost_parse=BoostDataParser(raw_data)
    #print(boost_parse.doc_content)
    boost_parse.filter_data(boost_parse.doc_content)
    boost_parse.parse_final_data()
    #print(boost_parse.parse_name_and_anchor(boost_parse.filtered_doc))
    #print(boost_parse.parse_first_paragraph(boost_parse.filtered_doc))
    
    print(boost_parse.get_data())
