import os
from bs4 import BeautifulSoup
URL_ROOT = 'http://www.boost.org'

URL_DOC = 'http://www.boost.org/doc/libs/1_63_0/'


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


        self.filter_data(self.doc_content)


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
            

class BoostDataOutput:

    def __init__(self,data):

        self.data = data

    def write_file(self):


         with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    name = data_element.get('name')
                    first_paragraph = '<p>' + data_element.get('first_paragraph') + '</p>'
                    abstract = first_paragraph
                    abstract = '<section class="prog__container">' + abstract + '</section>'
                    url = '{}'.format(data_element.get('anchor'))
                    list_of_data = [
                        name,       # 1. article title
                        'A',        # 2.type is article
                        '',         # 3.no redirect data
                        '',         # 4.leave empty
                        '',         # 5.no categories
                        '',         # 6.leave empty
                        '',         # 7.no related topics
                        '',         # 8.leave empty
                        URL_DOC,    # 9.an external link back to home
                        '',         # 10.no disambiguation
                        '',         # 11.images
                        abstract,   # 12.abstract
                        url         # 13.url to doc
                    ]

                    output_file.write('{}\n'.format('\t'.join(list_of_data)))


        
if __name__=="__main__":
    
    boost_data=BoostData()
    raw_data=boost_data.get_raw_data()
    boost_parse=BoostDataParser(raw_data)
    boost_parse.parse_final_data()
    output=BoostDataOutput(boost_parse.get_data())
    output.write_file()
