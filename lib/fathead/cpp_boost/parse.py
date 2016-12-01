import os
from bs4 import BeautifulSoup
URL_ROOT = 'http://www.boost.org/'

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

        self.parsed_data=None
        
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

        name=''
        anchor=''

    def filter_data(self,section):

        for dl_elem in section.find_all('dl',{'class':'fields'}):
            dt=dl_elem.find_all('dd')
            for dt_elem in dt:
                dt_elem.replaceWith('')
        print(section)
                
            
        
        






if __name__=="__main__":
    
    boost_data=BoostData()
    raw_data=boost_data.get_raw_data()
    #print(raw_data)
    boost_parse=BoostDataParser(raw_data)
    #print(boost_parse.doc_content)
    boost_parse.filter_data(boost_parse.doc_content)
    
