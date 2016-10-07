from bs4 import BeautifulSoup

SASS_DOC_BASE_URL = 'http://sass-lang.com/documentation/file.SASS_REFERENCE.html'
DOWNLOADED_HTML_PATH = 'download/file.SASS_REFERENCE.html'


class Data(object):
    """
    Object responsible for loading raw HTML data from Sass Reference
    """
    def __init__(self, file):
        """
        Initialize Data object. Load data from HTML.

        """
        self.HTML = ""
        self.FILE = file
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.

        """
        with open(self.FILE, 'r') as data_file:
            self.HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.

        """
        return self.HTML

    def get_file(self):
        """
        Returns: The file path of the file being used.

        """
        return self.FILE


class DataParser(object):
    """
    Object responsible for parsing the raw HTML that contains data
    """
    def __init__(self, data_object, titles):
        """
        Given raw data, get the relevant sections
        Args:
            raw_data: HTML data
        """
        self.titles = titles
        self.parsed_data = None
        self.topic_sections = []
        self.file_being_used = data_object.get_file()

        self.soup_data = BeautifulSoup(data_object.get_raw_data(), 'html.parser')
        table_of_contents = self.soup_data.find(class_="maruku_toc")
        sections = table_of_contents.find_all('li')
        for section in sections:
                section_id = section.find('a')
                section_id = section_id['href']
                heading = self.soup_data.find(id=section_id[1:])
                self.topic_sections.append(heading)

                                          
    def parse_for_name(self, section):
        """
        Returns the section name
        Args:
            section: A section of parsed HTML that represents a topic

        Returns:
            Name of topic

        """
        name = section.text
        if name in self.titles.keys():
            info = self.titles[name]
            if info[0].strip() != 'None':
                return info[0].strip()
            else:
                return name
        else:
            return None
    def parse_for_redirects(self, section):
        """
        Returns any redirects for article 
        Args:
            section: A section of parsed HTML that represents a topic

        Returns:
            list of redirects

        """
        name = section.text
        if name in self.titles.keys():
            info = self.titles[name]
            if info[1].strip() != 'None':
                return info[1].strip().split(',')
            else:
                return []
        else:
            return []
    def parse_for_id(self, section):
        """
        Returns the section id for topic
        Args:
            section: A section of parsed HTML that represents a topic

        Returns:
            id of section

        """
        return '#'+ section.get('id')

    def parse_for_description(self, section):
        """
        Returns the topic description
        Fixes up some weird double spacing and newlines.
        Args:
            section: A section of parsed HTML that represents a topic

        Returns:
            topic description

        """
        next_para = section.find_next('p')
        description = "<p>" + str(next_para.text.encode('utf-8')) + "</p>"
        next_tag  = next_para.find_next_sibling()
        if next_tag.name=="pre" or next_tag.name=="code":
            text = str(next_tag.encode('utf-8'))
            text = '\\n'.join(text.split('\n'))
            description = description + text
        return description

    def create_url(self, id):
        """
        Helper method to create URL back to document
        Args:
            anchor: #anchor

        Returns:
            Full URL to function on the sass doc

        """
        return SASS_DOC_BASE_URL + id

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []
        names = []
        for topic_section in self.topic_sections:
            name = self.parse_for_name(topic_section)
            if name:
                description = self.parse_for_description(topic_section)
                id = self.parse_for_id(topic_section)
                url = self.create_url(id)
                redirect = self.parse_for_redirects(topic_section)
                
                if name in names:
                    index = names.index(name)
                    data_elements = data[index]
                    data_elements['description'] += description
                    data_elements['redirects'].extend(redirect)
                else:
                    names.append(name)
                    data_elements = {
                        'name': name,
                        'description': description,
                        'url': url,
                        'redirects' : redirect
                    }

                    data.append(data_elements)

        self.parsed_data = data

    def get_data(self):
        """
        Get the parsed data.
        Returns:
            self.parsed_data: Dict containing necessary data elements
        """
        return self.parsed_data


class DataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        self.data = data

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'a') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    description = '<div class="prog__container">' + data_element.get('description') + '</div>'
                    url = data_element.get('url').encode('utf-8')
                    name = data_element.get('name').encode('utf-8')
                    redirect = data_element.get('redirects')
                    list_of_data = [
                        name,                       # unique name
                        'A',                        # type is article
                        '',                         # no redirect data
                        '',                         # ignore
                        '',                         # no categories
                        '',                         # ignore
                        '',                         # no related topics
                        '',                         # ignore
                        '',                         # external link 
                        '',                         # no disambiguation
                        '',                         # images
                        description,                # abstract
                        url                         # url to doc
                    ]
                    line = '\t'.join(list_of_data)
                    output_file.write(line+'\n')
    def create_redirects(self):
        """
        Iterate through the data and create the needed output.txt file, appending to file as necessary.

        """
        with open('output.txt', 'a') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    name = data_element.get('name').encode('utf-8')
                    redirects = data_element.get('redirects')
                    for redirect in redirects:
                        list_of_data = [
                            redirect.strip(),           # unique name
                            'R',                        # type is article
                            name,                       # redirect data
                            '',                         # ignore
                            '',                         # no categories
                            '',                         # ignore
                            '',                         # no related topics
                            '',                         # ignore
                            '',                         # external link 
                            '',                         # no disambiguation
                            '',                         # images
                            '',                         # abstract
                            '',                         # url to doc
                        ]
                        line = '\t'.join(list_of_data)
                        output_file.write(line+'\n')
def getTitleInfo():
    """
    Read through titles.txt and return title names and redirect information.
    """
    titles = {}
    with open('titles.txt','r') as f:
        for line in f:
            line = line.split('    ')
            if line[1].strip()!="N":
                titles[line[0]] = [line[2], line[3].strip()]
    return titles
    

if __name__ == "__main__":
    file_path = 'download/file.SASS_REFERENCE.html'
    title_info = getTitleInfo()
    data = Data(file_path)
    parser = DataParser(data, title_info)
    parser.parse_for_data()
    output = DataOutput(parser.get_data())
    output.create_file()
    output.create_redirects()