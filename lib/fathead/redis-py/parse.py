from bs4 import BeautifulSoup, NavigableString

REDIS_PY_HOME = 'https://redis-py.readthedocs.io/en/latest/'

class SoupData(object):
    """
    Object responsible for loading raw HTML data from BeautifulSoup doc:
    """

    def __init__(self):
        """
        Initialize SoupData object. Load data from HTML.
        """
        self.SOUP_HTML = ""
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.
        """
        with open('download/index.html', 'r') as data_file:
            self.SOUP_HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.
        """
        return self.SOUP_HTML

class SoupDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains the beautifulsoup data
    """

    def __init__(self, raw_data):
        """
        Given raw data, get the relevant section
        Args:
            raw_data: HTML data
        """

        self.parsed_data = None
        self.classes = None

        soup = BeautifulSoup(raw_data, 'html.parser')
        self.classes = soup.findAll('dl', {'class': ['class', 'function']})

    def parse_name_and_anchor_from_data(self, section):
        """
        Find the name and anchor for a given section
        Args:
            section: A section of parsed HTML
        Returns:
            name: Name of the section
            anchor: Anchor tag to use when linking back to the docs (ie #tags)
        """

        name, anchor = None, None
        a_tag = section.find('a', {'class': 'headerlink'})
        name = section['id']
        if a_tag:
            anchor = a_tag['href']
        return name, anchor

    def parse_code_from_data(self, section):
        """
        Look for example code block to output
        Args:
            section: A section of parsed HTML
        Returns:
            Formatted code string
        """

        code = section.get_text()
        if code:
            return '<pre><code>{}</code></pre>'.format(code.replace('[source]', '').replace('¶', '').replace('\n', '\\n'))
        return None

    def parse_paragraph_from_data(self, section):
        """
        Get the required paragraph for display
        Args:
            section: A section of parsed HTML
        Returns:
            paragraph in the HTML
        """

        data = ""
        for tag in section:
            if not tag or isinstance(tag, NavigableString):
                continue
            if tag.name != 'p':
                break
            data += '<p>{}</p><br>'.format(tag.get_text())
        return data

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements
        """

        data = list()
        class_data, class_desc = list(), list()

        for section in self.classes:
            for sub_section in section:
                if isinstance(sub_section, NavigableString):
                	continue
                if sub_section:
                    if len(class_data) > len(class_desc):
                        class_desc.append(sub_section)
                    else:
                        class_data.append(sub_section)

        for section in zip(class_data, class_desc):
            class_name, class_anchor = self.parse_name_and_anchor_from_data(section[0])
            class_code = self.parse_code_from_data(section[0])
            class_para = self.parse_paragraph_from_data(section[1])
            data_elements = {
                'name': class_name,
                'anchor': class_anchor,
                'paragraph': class_para,
                'code': class_code
            }
            data.append(data_elements)

            for sub_section in section[1].find_all('dl', {'class': 'method'}):
                method_data, method_desc = list(), list()
                for child in sub_section.children:
                    if not child or isinstance(child, NavigableString):
                        continue
                    if len(method_data) > len(method_desc):
                        method_desc.append(child)
                    else:
                        method_data.append(child)
                for method in zip(method_data, method_desc):
                    name, anchor = self.parse_name_and_anchor_from_data(method[0])
                    para = self.parse_paragraph_from_data(method[1])
                    code = self.parse_code_from_data(method[0])
                    data_elements = {
                        'name': name,
                        'anchor': anchor,
                        'paragraph': para,
                        'code': code
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

class SoupDataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        """
        Initialize SoupDataOutput object
        Args:
            data: Dict containing the data elements
        """
        self.data = data

    def create_file(self):
        """
        Iterate through the data and create the necessary output.txt file
        """

        with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                name = data_element.get('name').lower()
                code = data_element.get('code')
                paragraph = data_element.get('paragraph')
                abstract = '<div class="prog__container">{}{}{}</div>'.format(
                    code, '<br>' if code else '', paragraph)
                url = '{}{}'.format(REDIS_PY_HOME, data_element.get('anchor'))
                list_of_data = [
                    name,        #unique name
                    'A',         #type is article
                    '',          #no redirect data
                    '',          #ignore
                    '',          #no categories
                    '',          #ignore
                    '',          #no related topics
                    '',          #ignore
                    REDIS_PY_HOME,   #add an external link back to BeautifulSoup Home
                    '',          #no disambiguation
                    '',          #images
                    abstract,    #abstract
                    url          #url to the relevant tag doc
                ]
                output_file.write('{}\n'.format('\t'.join(list_of_data)))

if __name__ == "__main__":
    data = SoupData()
    parser = SoupDataParser(data.get_raw_data())
    parser.parse_for_data()
    output = SoupDataOutput(parser.get_data())
    output.create_file()
