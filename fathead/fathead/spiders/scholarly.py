from scrapy.spider import BaseSpider
from logging import INFO
from csv import reader
from cStringIO import StringIO
import re
from urllib import urlencode
from json import loads
from fathead.items import FatheadItem



def format_keywords(keywords):
    linked_kw = [kw.lower() for kw in keywords]
    first_part = ', '.join(linked_kw[:-2])
    second_part = ' and '.join(linked_kw[-2:])
    parts = [part for part in [first_part, second_part] if len(part) > 0]
    return ', '.join(parts)


class ScholrlySpider(BaseSpider):
    name = "scholrly"
    WHITESPACE_PATTERN = re.compile(r'\s+')
    allowed_domains = ["https://s3.amazonaws.com/"]
    start_urls = [
        "https://s3.amazonaws.com/scholrly-external/ddg-2013-4-3.tsv",
        ]
    
    def nf(self, num, keyword):
        if num == '1':
            return '%s %s' % (num, keyword)
        else:
            return '%s %ss' % (num, keyword)

    def parse(self, response):
        csv = reader(StringIO(response.body_as_unicode()), delimiter='\t')
        for name, url, image_url, num_papers, num_coauthors, \
                citations, keywords in csv:
            PAT = re.compile(r'\"([\w\s\.]+)\"')
            name = PAT.findall(name)
            keywords = PAT.findall(keywords)
            ABSTRACT_TEMPLATE = unicode("""
{name} is a researcher{keyword_phrase}. {name} has written {paper_details} with {coauthor_details} and {citation_details}.
""")
            
            keyword_phrase = ' interested in %s' % format_keywords(keywords) if len(keywords) > 0 else ''

            item = FatheadItem()
            if len(name) < 1:
                continue
            item['title'] = name[0]
            item['_type'] = 'A'
            item['categories'] = 'researchers'
            item['external_links'] = '[%s More at Scholrly]' % url
            item['images'] = '[[Image:%s]]' % image_url
            item['abstract'] = ABSTRACT_TEMPLATE.format(
                name=name[0],
                paper_details = self.nf(num_papers, 'paper'),
                coauthor_details = self.nf(num_coauthors, 'coauthor'),
                citation_details = self.nf(citations, 'citation'),
                keyword_phrase = keyword_phrase,
                )
            item['source_url'] = url
            yield item
            for n in name[1:]:
                item = FatheadItem()
                item['title'] = n
                item['_type'] = 'R'
                item['redirect'] = name[0]
                yield item
        
