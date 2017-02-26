#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import re

from bs4 import BeautifulSoup

class PyMongoParser():
    def __init__(self, baseurl, downloaddir):
        """
        Parses downloaded PyMongo API documentation files
        :param baseurl: BaseURL of api docs
        :param downloaddir: Directory of downloaded API docs
        """
        self.baseurl = baseurl
        self.downloaddir = downloaddir
        self.api_files = []
        self.get_api_doc_files()
        self.output = ""
        self.print_output()

    def get_api_doc_files(self):
        """
        Reads index.html of downloaded API docs and parses for links to api docs files
        """
        self.api_files = []
        entry_html = open(self.downloaddir + 'index.html')
        soupy = BeautifulSoup(entry_html, 'html.parser')
        for element in soupy.find_all("a", {"class": "reference internal", "href": re.compile('^.*\.html$')}):
            self.api_files.append(self.downloaddir + element["href"])
        del soupy

    def print_output(self):
        """
        Parses found files, prints formatted output for DDG Fathead to stdout.
        """
        for filename in self.api_files:
            html_data = open(filename).read()
            soupy = BeautifulSoup(html_data, 'html.parser')
            # Get any classes in the file
            for element in soupy.find_all("dl", {"class": "class"}):
                anchor_link = ""
                if element.find("a", {"class": "headerlink"}):
                    anchor_link = element.find("a", {"class": "headerlink"})["href"]
                code = ""
                description = ""
                # Format methods/classmethods/functions/attributes/exceptions within a class
                tags_to_replace = ['method', 'classmethod', 'function', 'attribute', 'exception', 'describe']
                for tag_to_replace in tags_to_replace:
                    for tag in element.find_all("dl", {"class": tag_to_replace}):
                        tag_code = ""
                        tag_description = ""
                        for item in tag.dt.contents:
                            tag_code += str(item)
                        tag_code = self._clean_html_tags(tag_code)
                        for item in tag.dd.contents:
                            tag_description += str(item)
                        tag_description = self._clean_html_tags(tag_description)
                        description += tag_code
                        description += tag_description
                        tag.decompose()
                output_line = ""
                for item in element.dt.contents:
                    code += str(item)
                code = self._clean_html_tags(code)
                code = self._clean_code_tags(code)
                for item in element.dd.contents:
                    description += str(item)

                description = self._clean_html_tags(description)

                abstract = '<section class="prog_container">' + \
                           '<pre><code>' + code + '</pre></code><p>' + \
                           description + '</p></section>'
                title = element.dt["id"].replace('.', ' ')

                filename_removed_dir = filename.replace('download/', '')
                headerlink = self.baseurl + filename_removed_dir + anchor_link
                # 1. Full article title
                output_line += title + "\t"
                # 2. Type of entry (A for article)
                output_line += "A" + "\t"
                # 3. For redirects only
                output_line += "" + "\t"
                # 4. Empty
                output_line += "" + "\t"
                # 5. Categories
                output_line += "" + "\t"
                # 6. Empty
                output_line += "" + "\t"
                # 7. Related topics
                output_line += "" + "\t"
                # 8. Empty
                output_line += "" + "\t"
                # 9. External Links (official site) [$url link_text]
                output_line += "" + "\t"
                # 10. Disambiguation pages only
                output_line += "" + "\t"
                # 11. Image
                output_line += "" + "\t"
                # 12. Abstract (Main content)
                output_line += abstract + "\t"
                # 13. Full URL to source (url with anchor)
                output_line += headerlink
                print(output_line)
                element.decompose()

            # get anything else that may be in there, not in a class
            tags_to_replace = ['method', 'classmethod', 'function', 'attribute', 'exception', 'describe', 'data']
            for tag_to_replace in tags_to_replace:
                for tag in soupy.find_all("dl", {"class": tag_to_replace}):
                    anchor_link = ""
                    if tag.find("a", {"class": "headerlink"}):
                        anchor_link = tag.find("a", {"class": "headerlink"})["href"]
                    tag_code = ""
                    tag_description = ""
                    for item in tag.dt.contents:
                        tag_code += str(item)
                    tag_code = self._clean_html_tags(tag_code)
                    tag_code = self._clean_code_tags(tag_code)
                    for item in tag.dd.contents:
                        tag_description += str(item)
                    tag_description = self._clean_html_tags(tag_description)

                    output_line = ""
                    abstract = '<section class="prog_container">' + \
                                '<pre><code>' + tag_code + '</pre></code><p>' + \
                                tag_description + '</p></section>'
                    title = tag.dt["id"].replace('.', ' ')
                    tag.decompose()

                    filename_removed_dir = filename.replace('download/', '')
                    headerlink = self.baseurl + filename_removed_dir + anchor_link
                    # 1. Full article title
                    output_line += title + "\t"
                    # 2. Type of entry (A for article)
                    output_line += "A" + "\t"
                    # 3. For redirects only
                    output_line += "" + "\t"
                    # 4. Empty
                    output_line += "" + "\t"
                    # 5. Categories
                    output_line += "" + "\t"
                    # 6. Empty
                    output_line += "" + "\t"
                    # 7. Related topics
                    output_line += "" + "\t"
                    # 8. Empty
                    output_line += "" + "\t"
                    # 9. External Links (official site) [$url link_text]
                    output_line += "" + "\t"
                    # 10. Disambiguation pages only
                    output_line += "" + "\t"
                    # 11. Image
                    output_line += "" + "\t"
                    # 12. Abstract (Main content)
                    output_line += abstract + "\t"
                    # 13. Full URL to source (url with anchor)
                    output_line += headerlink
                    print(output_line)

    def _clean_code_tags(self, code_html_to_clean):
        """
        Cleans <code> tags from html. Should only be used in 'code" section to prevent
        <code> tags from doubling up within the <pre><code> section
        :param code_html_to_clean: String of html to remove code tags
        :return: html with removed code tags
        """

        html_soup_cleaner = BeautifulSoup(code_html_to_clean, 'html.parser')

        # Remove code tags so only the ones we want are displayed
        for tag in html_soup_cleaner.find_all("code"):
            if tag.attrs:
                del tag.attrs
            del tag.name

        cleaned_html = str(html_soup_cleaner)
        cleaned_html = cleaned_html.replace("<None>", '')
        cleaned_html = cleaned_html.replace("</None>", '')
        cleaned_html = cleaned_html.replace("<none>", '')
        cleaned_html = cleaned_html.replace("</none>", '')

        return cleaned_html

    def _clean_html_tags(self, html_to_clean):
        """
        Cleans unwanted tags from html for DDG Fathead output
        :param html_to_clean: string of html to clean
        :return: string of cleaned html
        """
        html_soup_cleaner = BeautifulSoup(html_to_clean, 'html.parser')

        # Remove code tags so only the ones we want are displayed
        for tag in html_soup_cleaner.find_all("code"):
            if tag.attrs:
                del tag.attrs

        # Replace code tag from documentation to expected
        for tag in html_soup_cleaner.find_all("div", {"class": "highlight-default"}):
            tag.name = "pre"
            del tag.attrs
            tag.next_element.name = "code"
            del tag.next_element.attrs
            tag.next_element.next_element.name = "span"

        # replace the version_modified spans with italics
        for tag in html_soup_cleaner.find_all("span", {"class": "versionmodified"}):
            tag.name = "i"
            if tag.attrs:
                del tag.attrs

        # Remove tags we don't want
        tags_to_replace = ['span', 'div', 'blockquote']
        for tag_to_replace in tags_to_replace:
            regex_pattern = "^{}*".format(tag_to_replace)
            for tag in html_soup_cleaner.find_all(re.compile(regex_pattern)):
                del tag.name
                if tag.attrs:
                    del tag.attrs

        # Remove links
        for tag in html_soup_cleaner.find_all("a"):
            if tag.attrs:
                del tag.name
                del tag.attrs

        # Remove any formatting attributes from tags we want
        tags_to_replace = ['th', 'td', 'tl', 'tr', 'table', 'p', 'dd', 'dt', 'col', 'tbody']
        for tag in html_soup_cleaner.find_all(tags_to_replace):
            if tag.attrs:
                del tag.attrs

        for tag in html_soup_cleaner.find_all("ul"):
            if tag.attrs:
                del tag.attrs

        for tag in html_soup_cleaner.find_all("em", {"class": "property"}):
            if tag.attrs:
                del tag.attrs

        # Change <cite> tags to more relevant <code> tags
        for tag in html_soup_cleaner.find_all("cite"):
            tag.name = "code"

        cleaned_html = str(html_soup_cleaner)
        cleaned_html = cleaned_html.replace("¶", "")
        cleaned_html = cleaned_html.replace("<None>", '')
        cleaned_html = cleaned_html.replace("</None>", '')
        cleaned_html = cleaned_html.replace("<none>", '')
        cleaned_html = cleaned_html.replace("</none>", '')
        cleaned_html = cleaned_html.replace('\n', '\\n').strip('\n')

        return cleaned_html

def main():
    """
    Calls PyMongoParser to parse and output API documentation
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--downloaddir", help="Directory of downloads", default="download/", required=True)
    parser.add_argument("-u", "--baseurl", help="Entry URL to parse for class documentation", required=True)
    args = parser.parse_args()

    pymongo_parse = PyMongoParser(args.baseurl, args.downloaddir)

if __name__ == '__main__':
    main()
