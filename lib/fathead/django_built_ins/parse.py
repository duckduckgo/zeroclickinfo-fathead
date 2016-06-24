import argparse

from bs4 import BeautifulSoup

ARGUMENT_PARSER = argparse.ArgumentParser()
ARGUMENT_PARSER.add_argument('--django-version')

DJANGO_HOME = 'https://www.djangoproject.com/'
DJANGO_DOC_URL = 'https://docs.djangoproject.com/en/{}/ref/templates/builtins/'


class DjangoData(object):
    """
    Object responsible for loading raw HTML data for Django templatetags/filters:
    """
    def __init__(self):
        """
        Initialize DjangoData object. Load data from HTML.

        """
        self.DJANGO_HTML = ""
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.

        """
        with open('download/index.html', 'r') as data_file:
            self.DJANGO_HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.

        """
        return self.DJANGO_HTML


class DjangoDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains Django templatetag/filter data
    """
    def __init__(self, raw_data):
        """
        Given raw data, get the relevant templatetag section and the filter section
        Args:
            raw_data: HTML data
        """
        self.parsed_data = None

        soup_data = BeautifulSoup(raw_data, 'html.parser')
        doc_content = soup_data.find('div', {'id': 'docs-content'})

        tags = doc_content.find('div', {'class': 'section', 'id': 's-built-in-tag-reference'})
        self.tag_sections = tags.find_all('div', {'class': 'section'})

        filters = doc_content.find('div', {'class': 'section', 'id': 's-built-in-filter-reference'})
        self.filter_sections = filters.find_all('div', {'class': 'section'})

    def parse_name_and_anchor_from_data(self, section):
        """
        Find the name and anchor for a given templatetag/filter
        Args:
            section: A section of parsed HTML that represents a templatetag or filter

        Returns:
            name: Name of the templatetag or filter
            anchor: Anchor tag to use when linking back to docs (ie #autoescape)

        """
        name = ''
        anchor = ''
        h3 = section.find('h3')
        if h3:
            code = h3.find('code', {'class': 'docutils'})
            if code:
                name = code.find('span', {'class': 'pre'}).string
            a_tag = h3.find('a', {'class': 'headerlink'})
            if a_tag:
                anchor = a_tag['href']

        return name, anchor

    def parse_first_paragraph_from_data(self, section):
        """
        Get the first paragraph for display
        Args:
            section: A section of parsed HTML that represents a templatetag or filter

        Returns:
            First paragraph in the HTML

        """
        return section.find('p').text.replace('\n', ' ')

    def parse_code_from_data(self, section):
        """
        Look for an example code block to output
        Args:
            section: A section of parsed HTML that represents a templatetag or filter

        Returns:
            Formatted code string
        """
        code = section.find('div', {'class': 'highlight'})
        if code:
            return '<pre><code>{}</code></pre>'.format(code.text.replace('\n', '<br>'))
        return ''

    def parse_for_data(self):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        data = []

        for section in (self.tag_sections + self.filter_sections):
            name, anchor = self.parse_name_and_anchor_from_data(section)
            first_paragraph = self.parse_first_paragraph_from_data(section)
            code = self.parse_code_from_data(section)

            data_elements = {
                'name': name,
                'anchor': anchor,
                'first_paragraph': first_paragraph,
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


class DjangoDataOutput(object):
    """
    Object responsible for outputting data into the output.txt file
    """
    def __init__(self, data):
        self.data = data

    def create_file(self):
        """
        Iterate through the data and create the needed output.txt file.

        """
        with open('output.txt', 'w+') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    name = data_element.get('name')
                    abstract = '{}<br>{}'.format(data_element.get('code'), data_element.get('first_paragraph'))
                    url = '{}{}'.format(DJANGO_DOC_URL, data_element.get('anchor'))
                    list_of_data = [
                        name,       # unique name will be the name of the templatetag/filter
                        'A',        # type is article
                        '',         # no redirect data
                        '',         # ignore
                        '',         # no categories
                        '',         # ignore
                        '',         # no related topics
                        '',         # ignore
                        DJANGO_HOME,# add an external link back to Django home
                        '',         # no disambiguation
                        '',         # images
                        abstract,   # abstract
                        url         # url to templatetag/filter doc
                    ]
                    output_file.write('{}\n'.format('\t'.join(list_of_data)))


if __name__ == "__main__":
    args = ARGUMENT_PARSER.parse_args()
    if args.django_version:
        DJANGO_DOC_URL = DJANGO_DOC_URL.format(args.django_version)
    else:
        DJANGO_DOC_URL = DJANGO_DOC_URL.format('1.9')

    data = DjangoData()
    parser = DjangoDataParser(data.get_raw_data())
    parser.parse_for_data()
    output = DjangoDataOutput(parser.get_data())
    output.create_file()
