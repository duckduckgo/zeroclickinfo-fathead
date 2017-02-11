#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
import os
import re

from bs4 import BeautifulSoup
import requests


class CoverageClass():
    def __init__(self, raw_data, base_url, base_title):
        """
        CoverageClass - Class object scraped from Coverage API
        documentation of a class
        :param raw_data: HTML of class scraped from documentation
        :param base_url: URL of API Documentation
        """
        self.raw_data = raw_data
        self.base_url = base_url
        self.base_title = base_title
        self.method_list = []
        self.class_name = ''
        self.parse_raw_class_data_into_methods()

    def get_method_list(self):
        """
        Used to get list of methods parsed from class
        :return: List of methods parsed from class
        """
        return self.method_list

    def get_method_count(self):
        """
        Used to get number of methods
        :return: number of methods
        """
        return len(self.method_list)

    def parse_raw_class_data_into_methods(self):
        """
        Scrapes methods from html of class object
        """
        soupy = BeautifulSoup(self.raw_data, 'html.parser')
        self.class_name = soupy.dt["id"]
        methods = soupy.find_all('dl', {'class': 'method'})
        for method in set(methods):
            self.method_list.append(method)

    def get_raw_method(self, method_index):
        """
        Returns method from method list at method_index
        :param method_index: Index of method to return raw data
        :return: raw html data of method
        """
        return self.method_list[method_index]

    def _clean_html_tags(self, html_to_clean):
        """

        """

        html_soup_cleaner = BeautifulSoup(html_to_clean, 'html.parser')
        # Replace code tag from documentation to expected
        for tag in html_soup_cleaner.find_all("div", {"class": "highlight-default"}):
            tag.name = "pre"
            del tag.attrs
            tag.next_element.name = "code"
            del tag.next_element.attrs
            tag.next_element.next_element.name = "span"

        # Remove tags we don't want
        tags_to_replace = ['code', 'span', 'div', 'blockquote']
        for tag_to_replace in tags_to_replace:
            regex_pattern = "^{}*".format(tag_to_replace)
            for tag in html_soup_cleaner.find_all(re.compile(regex_pattern)):
                if tag.name == "a":
                    print(tag)
                del tag.name
                if tag.attrs:
                    del tag.attrs
        # Remove links
        for tag in html_soup_cleaner.find_all("a"):
            if tag.attrs:
                del tag.name
                del tag.attrs

        # Remove any formatting class specified for table
        for tag in html_soup_cleaner.find_all("ul"):
            if tag.attrs:
                del tag.attrs

        cleaned_html = str(html_soup_cleaner)
        cleaned_html = cleaned_html.replace("<None>", '')
        cleaned_html = cleaned_html.replace("</None>", '')
        cleaned_html = cleaned_html.replace('\n', '\\n').strip('\\n ')

        return cleaned_html

    def get_parsed_method_from_data(self, method):
        """
        Parses method html into json object
        :param method: raw html data of method
        :return: json object of parsed values
        """
        attributes = {}

        html_soup = BeautifulSoup(str(method), 'html.parser')
        attributes["base_url"] = self.base_url
        attributes["base_title"] = self.base_title
        attributes["id"] = html_soup.dt["id"]
        attributes["title"] = attributes["id"].split('.')[1] + " " + \
            attributes["id"].split('.')[2]
        attributes["headerlink"] = "{}{}".format(self.base_url,
                                                 html_soup.a["href"])

        attributes["code"] = ''
        # find the line of code in the method
        code_line = re.search('^<code.*\n', method, re.MULTILINE).group(0)
        # grab everything starting with <code to the last </span> tag, which
        # is the closing paren
        code_line = code_line[0:code_line.rindex('</span>') + len('</span>')]
        attributes["code"] = self._clean_html_tags(code_line)

        temp_desc = ''
        for index, value in enumerate(html_soup.dd):
            temp_desc += str(value)

        attributes["description"] = self._clean_html_tags(temp_desc)
        attributes["abstract"] = '<section class="prog_container">' + \
                                 '<pre><code>' + attributes["code"] + \
                                 '</pre></code><p>' +\
                                 attributes["description"] + \
                                 '</p></section>'
        return attributes

    def get_parsed_method_suitable_for_output(self, method_index):
        """
        Takes attributes from method, outputs as expected for DDG fathead's output.txt
        :param method_index: int index of method to parse for output
        :return: single line string of parsed output
        """
        output_line = ""
        attributes = self.get_parsed_method_from_method_index(method_index)
        # 1. Full article title
        output_line += attributes["title"] + "\t"
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
        output_line += attributes["abstract"] + "\t"
        # 13. Full URL to source (url with anchor)
        output_line += attributes["headerlink"] + "\n"
        return output_line

    def get_parsed_method_from_method_index(self, method_index):
        """
        Return parsed method using method index
        :param method_index: index in method list of method to parse
        :return: json object of parsed method
        """
        raw_method = str(self.get_raw_method(method_index))
        return self.get_parsed_method_from_data(raw_method)


class CoverageParser():
    def __init__(self,
                 URL="https://coverage.readthedocs.io/en/latest/api.html"):
        """
        Given a URL of the Coverage API, will find
        and create CoverageClass objects
        :param URL: URL of entry to CoverageAPI Documentation
        """
        self.URL = URL
        self.classes = []
        self.api_urls = []

    def get_num_classes(self):
        return len(self.classes)

    def get_output(self):
        """
        Gets output for all methods in class
        :return: string parsed to output.txt format expected by DDG
        """
        output = ""
        for class_instance in self.classes:
            method_count = class_instance.get_method_count()
            for x in range(0, method_count - 1):
                output += class_instance.get_parsed_method_suitable_for_output(x)
        return output

    def get_html_data_from_url(self, url):
        """
        Parses URL for HTML
        :param url: URL to be parsed
        :return: json object of URL and html data
        """
        html_data = requests.get(url).text
        return_data = {
            "URL": url,
            "html_data": html_data
        }
        return return_data

    def get_html_data_from_file(self, url, filename):
        """
        Parses URL for HTML
        :param url: URL where file retrieved
        :param filename: filename of html data to parse
        :return: json object of URL and html data
        """
        html_data = open(filename).read()
        url = url[:url.rindex('/')]
        if filename.find('/'):
            filename = filename[filename.rindex('/') + 1:]
        url = "{}/{}".format(url, filename)
        return_data = {
            "URL": url,
            "html_data": html_data
        }
        return return_data

    def get_api_urls(self):
        """
        Sets/returns API URLs found in class HTML
        :return: API URLs found in parsing class
        """
        if self.api_urls == []:
            self.find_api_doc_urls(self.URL)
            return self.api_urls
        else:
            return self.api_urls

    def find_api_doc_urls(self, url):
        """
        Finds URLs from primary entry URL of documentation links
        :param url: URL to be parsed to retrieve documentation links
        :return: list of URLs which appear to be links to CoverageClass documentation
        """
        html_data = requests.get(url).text
        html_soup = BeautifulSoup(html_data, 'html.parser')
        url_base = url[0:url.rindex('/') + 1]

        api_url_list = []
        api_urls = html_soup.find_all('a', {'class': 'reference internal',
                                            'href': re.compile("^api.*\.html")},
                                      text=re.compile('.*class(es)?'))
        for url in set(api_urls):
            api_url_list.append("{}{}".format(url_base, url["href"]))
        self.api_urls = api_url_list

    def get_class_list(self):
        raise NotImplementedError

    def parse_data_for_classes(self, json_url_object):
        """
        Parses raw HTML data from URL of API for Coverage data classes, stores in object
        :param json_url_object: json object of URL and html_data to be parsed
        """
        html_soup = BeautifulSoup(json_url_object["html_data"], 'html.parser')
        title = html_soup.title.text
        class_data = html_soup.find_all('dl', {'class': 'class'})
        for class_instance in class_data:
            self.classes.append(CoverageClass(str(class_instance), json_url_object["URL"], title))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--downloaddir", help="Directory of downloads", default="download/")
    parser.add_argument("-l", "--listonly", help="Returns list of API documentation URLs only",
                        action="store_true")
    parser.add_argument("-u", "--baseurl", help="Entry URL to parse for class documentation")
    parser.add_argument("-w", "--output", help="File to write output")
    args = parser.parse_args()

    if args.listonly:
        # used to return the URLs to be downloaded
        coverage_parse = CoverageParser()
        for url in coverage_parse.get_api_urls():
            print(url)
        exit(0)

    if args.downloaddir and args.output:
        files = os.listdir(args.downloaddir)
        coverage_parsers = []
        for file in files:
            temp_coverage_parser = CoverageParser()
            html_json = temp_coverage_parser.get_html_data_from_file(args.baseurl,
                                                                     args.downloaddir +
                                                                     '/' + file)
            temp_coverage_parser.parse_data_for_classes(html_json)
            coverage_parsers.append(temp_coverage_parser)
            del temp_coverage_parser

        with open(args.output, 'w') as output_file:
            for coverage_parser in coverage_parsers:
                output_file.write(coverage_parser.get_output())


if __name__ == '__main__':
    main()
