import argparse
import os

from bs4 import BeautifulSoup

ARGUMENT_PARSER = argparse.ArgumentParser()
ARGUMENT_PARSER.add_argument('--django-version')

DJANGO_HOME = 'https://www.djangoproject.com/'
DJANGO_DOC_URL = 'https://docs.djangoproject.com/en/{}'


class DjangoData(object):
    """
    Object responsible for loading raw HTML data for Django Docs
    """

    def __init__(self, page_name):
        """
        Initialize DjangoData object. Load data from HTML.

        """
        self.DJANGO_HTML = ""
        self.load_data(page_name)

    def load_data(self, page_name):
        """
        Open the HTML file and load it into the object.

        """
        with open('download/' + page_name, 'r') as data_file:
            self.DJANGO_HTML = data_file.read()

    def get_raw_data(self):
        """
        Returns: The raw HTML that was loaded.

        """
        return self.DJANGO_HTML


class DjangoDataParser(object):
    """
    Object responsible for parsing the raw HTML that contains Django Docs data
    """

    def __init__(self, raw_data, section_name, page_url):
        """
        Given raw data, get the relevant sections
        Args:
            raw_data: HTML data
        """
        self.parsed_data = None
        self.url = page_url

        soup_data = BeautifulSoup(raw_data, 'html.parser')
        doc_content = soup_data.find('div', {'id': 'docs-content'})

        tags = doc_content.find(
            'div', {'class': 'section', 'id': section_name})
        self.tag_sections = tags.find_all('div', {'class': 'section'})

    def parse_name_and_anchor_from_data(self, section, heading_style):
        """
        Find the name and anchor for a given section
        Args:
            section: A section of parsed HTML that represents a section

        Returns:
            name: Name of the Element
            anchor: Anchor tag to use when linking back to docs (ie #autoescape)

        """
        name = ''
        anchor = ''
        heading = section.find(heading_style)
        if heading:
            code = heading.find('code', {'class': 'docutils'})
            if code:
                name = code.find('span', {'class': 'pre'}).string
            a_tag = heading.find('a', {'class': 'headerlink'})
            if a_tag:
                anchor = a_tag['href']

        return name, anchor

    def parse_first_paragraph_from_data(self, section, pstyle):
        """
        Get the first paragraph for display
        Args:
            section: A section of parsed HTML that represents a Element

        Returns:
            First paragraph in the HTML

        """

        if pstyle == "p":
            try:
                return section.find('p').text.replace('\n', ' ')
            except:
                return ''

        elif pstyle == "dt":
            try:
                dtname = section.find('dt').text.replace('\n', ' ')
                if "source" in dtname:
                    dtname = dtname[:-9]
                elif "¶" in dtname:
                    dtname = dtname[:-1]
                return dtname

            except:
                return ''

    def parse_second_paragraph_from_data(self, section, pstyle):
        """
        Get the second paragraph for display
        Args:
            section: A section of parsed HTML that represents a Element

        Returns:
            second paragraph in the HTML

        """

        if pstyle == "p":
            try:
                para = section.find_all('p')[1]
                para = para.text.partition(". ")
                return ''.join(para[:2]).replace('\n', ' ')
            except:
                return ''

        elif pstyle == "dt":
            para = section.find_all('p')[0]
            if para:
                para = para.text.partition(". ")
                return ''.join(para[:2]).replace('\n', ' ')
            else:
                return ''

    def parse_code_from_data(self, section):
        """
        Look for an example code block to output
        Args:
            section: A section of parsed HTML that represents a section

        Returns:
            Formatted code string
        """
        code = section.find('div', {'class': 'highlight'})
        if code:
            return '<pre><code>{}</code></pre>'.format(
                code.text.replace('\n', '\\n'))
        return ''

    def parse_for_data(self, code_or_second_para, hstyle, pstyle):
        """
        Main gateway into parsing the data. Will retrieve all necessary data elements.
        """
        parsing_data = []

        for section in (self.tag_sections):
            name, anchor = self.parse_name_and_anchor_from_data(
                section, hstyle)
            first_paragraph = self.parse_first_paragraph_from_data(
                section, pstyle)

            if code_or_second_para == "code":
                code = self.parse_code_from_data(section)
                second_paragraph = ''
            elif code_or_second_para == "para":
                second_paragraph = self.parse_second_paragraph_from_data(
                    section, pstyle)
                code = ''

            data_elements = {
                'name': name,
                'anchor': anchor,
                'first_paragraph': first_paragraph,
                'second_paragraph': second_paragraph,
                'code': code,
                'url': self.url
            }

            parsing_data.append(data_elements)

        self.parsed_data = parsing_data

    def get_data(self):
        """
        Get the parsed data.
        Returns:
            self.parsed_data: Dict containing necessary data elements
        """
        return self.parsed_data


class DjangoDataOutput(object):
    """
    Object responsible for outputting data into the output1.txt file
    """

    def __init__(self, data):
        self.data = data

    def create_file(self):
        """
        Iterate through the data and create the needed output1.txt file.

        """
        with open('output1.txt', 'a+') as output_file:
            for data_element in self.data:
                if data_element.get('name'):
                    name = data_element.get('name')
                    if "()" in name:
                        name = name[:-2]
                    code = data_element.get('code')
                    first_paragraph = '<p>' + \
                        data_element.get('first_paragraph') + '</p>'
                    second_paragraph = '<p>' + \
                        data_element.get('second_paragraph') + '</p>'
                    abstract = '{}{}{}'.format(
                        first_paragraph + second_paragraph, '', code)
                    abstract = '<section class="prog__container">' + abstract + '</section>'
                    url = '{}{}'.format(data_element.get(
                        'url'), data_element.get('anchor'))
                    list_of_data = [
                        name,       # unique name will be the name of the element
                        'A',        # type is article
                        '',         # no redirect data
                        '',         # ignore
                        '',         # no categories
                        '',         # ignore
                        '',         # no related topics
                        '',         # ignore
                        DJANGO_HOME,  # add an external link back to Django home
                        '',         # no disambiguation
                        '',         # images
                        abstract,   # abstract
                        url         # url to doc
                    ]
                    output_file.write('{}\n'.format('\t'.join(list_of_data)))


if __name__ == "__main__":
    args = ARGUMENT_PARSER.parse_args()
    if args.django_version:
        DJANGO_DOC_URL = DJANGO_DOC_URL.format(args.django_version)
    else:
        DJANGO_DOC_URL = DJANGO_DOC_URL.format('1.10')

    """
    The Complete Page Structure to be scrapped
        name: Downloaded file name
        sections: Sections in page
        code_or_second_para: Whether to get code or second_paragraph
        hstyle: heading attribute for the element
        pstyle: para attribute for the description
    """
    page_structure = [
        {"Name": 'contrib.html', "Sections": ['s-contrib-packages'],
         "Url": '/ref/contrib/', "code_or_second_para": "para", "hstyle": "h2", "pstyle": "p"},

        {"Name": 'django-admin.html', "Sections": ['s-available-commands'],
         "Url": '/ref/django-admin/', "code_or_second_para": "para", "hstyle": "h3", "pstyle": "dt"},

        {"Name": 'django-admin.html', "Sections": ['s-commands-provided-by-applications'],
         "Url": '/ref/django-admin/', "code_or_second_para": "para", "hstyle": "h4", "pstyle": "dt"},

        {"Name": 'exceptions.html', "Sections": ['s-module-django.core.exceptions', 's-url-resolver-exceptions', 's-http-exceptions', 's-transaction-exceptions'],
         "Url": '/ref/exceptions/', "code_or_second_para": "para", "hstyle": "h3", "pstyle": "dt"},

        {"Name": 'form_fields.html', "Sections": ['s-core-field-arguments'],
         "Url": '/ref/forms/fields/', "code_or_second_para": "para", "hstyle": "h3", "pstyle": "p"},

        {"Name": 'widgets.html', "Sections": ['s-built-in-widgets'],
         "Url": '/ref/forms/widgets/', "code_or_second_para": "para", "hstyle": "h4", "pstyle": "p"},

        {"Name": 'builtins.html', "Sections": ['s-built-in-tag-reference', 's-built-in-filter-reference'],
         "Url": '/ref/templates/builtins/', "code_or_second_para": "code", "hstyle": "h3", "pstyle": "p"},

        {"Name": 'settings.html', "Sections": ['s-core-settings', 's-auth', 's-messages', 's-sessions', 's-sites', 's-static-files'],
            "Url":'/ref/settings/', "code_or_second_para":"para", "hstyle":"h3", "pstyle":"p"},

        {"Name": 'validators.html', "Sections": ['s-built-in-validators'],
         "Url":'/ref/validators/', "code_or_second_para":"para", "hstyle":"h3", "pstyle":"p"},

        {"Name": 'urlresolvers.html', "Sections": ['s-module-django.urls'],
         "Url":'/ref/urlresolvers/', "code_or_second_para":"para", "hstyle":"h2", "pstyle":"p"},

        {"Name": 'urls.html', "Sections": ['s-module-django.conf.urls'],
         "Url":'/ref/urls/', "code_or_second_para":"para", "hstyle":"h2", "pstyle":"p"},

        {"Name": 'database-functions.html', "Sections": ['s-module-django.db.models.functions'],
         "Url":'/ref/models/database-functions/', "code_or_second_para":"para", "hstyle":"h2", "pstyle":"p"},

        {"Name": 'fields.html', "Sections": ['s-module-django.db.models.fields'],
         "Url":'/ref/models/fields/', "code_or_second_para":"para", "hstyle":"h3", "pstyle":"p"},

        {"Name": 'migration-operations.html', "Sections": ['s-schema-operations', 's-special-operations'],
         "Url":'/ref/migration-operations/', "code_or_second_para":"para", "hstyle":"h3", "pstyle":"dt"},

    ]

    for page in page_structure:
        data = DjangoData(page["Name"])

        page_url = '{}{}'.format(DJANGO_DOC_URL, page["Url"])

        parser = []
        for section_name in page["Sections"]:
            parser.append(DjangoDataParser(
                data.get_raw_data(), section_name, page_url))

            for parsed in parser:
                parsed.parse_for_data(page["code_or_second_para"], page[
                                      "hstyle"], page["pstyle"])
                output = DjangoDataOutput(parsed.get_data())
                output.create_file()



    # Removing duplicate entries...
    Dat = []
    with open('output1.txt', 'r') as file:
        for line in file:
            Dat.append(line)

    with open("output.txt", 'w') as file:
        for i in range(len(Dat)-1):
            l = Dat[i].split('\t', 1)[0]
            l2 = Dat[i+1].split('\t', 1)[0]
            if l != l2:
                file.write(Dat[i])
        file.write(Dat[len(Dat)-1])

    os.remove("output1.txt")
