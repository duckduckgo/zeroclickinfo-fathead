#!/usr/bin env python
# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from glob import glob
from urllib.parse import urljoin


def create_article(title, abstract, url):
    print('Title %s ' % title)
    print('URL %s' % url)
    print('Description %s' % abstract)
    data = [
            title,           # title
            'A',             # type is article
            '',              # no redirect data
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            abstract,        # abstract
            url              # anchor to specific section
        ]
    return '\t'.join(data)


def create_redirect(redirect_title, original_title):
    data = [
            redirect_title,  # title
            'R',             # type is article
            original_title,  # redirect title
            '',              # ignore
            '',              # no categories
            '',              # ignore
            '',              # no related topics
            '',              # ignore
            '',              # external link
            '',              # no disambiguation
            '',              # images
            '',              # abstract
            ''               # anchor to specific section
        ]
    return '\t'.join(data)


def parse_dl(dl, page_url):
    '''Parse dl containing function definitions for requests api.

    Accepts a dl and page_url.
    Returns list of articles and or redirect entries
    '''
    dt = dl.find('dt')
    func_with_params = dt.text.replace('¶', '').replace('[source]', '')
    func_without_params = dt.get('id')
    permalink = urljoin(page_url, '#{}'.format(func_without_params))
    module_func = ' '.join(func_without_params.split('.'))
    print(func_with_params)
    print(func_without_params)
    print(module_func)
    print(permalink)
    dd = dl.find('dd')
    abstract = ''
    if dd.p:
        abstract = dd.p.text
        table_params = dl.find('table')
        if table_params:
            for tr in table_params.findAll('tr'):
                abstract += ' {} '.format(tr.th.text)
                for li in tr.td.findAll('li'):
                    li_text = li.text.strip()
                    abstract += li_text
                else:
                    abstract += tr.td.text.strip()
        abstract = abstract.replace('\n', '')
        abstract = '<p>{}</p>'.format(abstract)
    code = ''
    if dl.pre:
        code = dl.pre.text.replace('\n', '\\n')
        code = '<pre><code>{}</code></pre>'.format(code)
        abstract += code
    if abstract:
        abstract = '<section>{}</section>'.format(abstract)
        print(abstract)


def parse_h2(h2_parent, page_url):
    '''Extracts article details from h2.

    Accepts h2_parent and extracts title, url, abstract and code snippets.
    Returns a list
    '''
    h2 = h2_parent.find('h2')
    title = h2.text.replace('¶', '')
    fragment = h2_parent.find('a').get('href')
    url = urljoin(page_url, fragment)
    abstract = ''
    next_sibling = h2.find_next_sibling(text=None)
    while next_sibling:
        if next_sibling.name == 'p':
            next_sibling_text = next_sibling.text.replace('\n', ' ')
            abstract += '<p>{}</p>'.format(next_sibling_text)
        elif next_sibling.name == 'div':
            pre = next_sibling.find('pre')
            if pre:
                pre_text = pre.text
                if title.startswith('Raw Response'):
                    last_span = pre.findAll('span')[-1]
                    original_span_text = last_span.text
                    escaped_span_text = last_span.text
                    escaped_span_text = bytes(escaped_span_text,
                                              encoding='UTF-8')
                    pre_text = pre.text.replace(original_span_text,
                                                '{}'.format(escaped_span_text))
                pre_text = pre_text.replace('\n', '\\n')
                abstract += '<pre><code>{0}</code></pre>'.format(pre_text)
        next_sibling = next_sibling.find_next_sibling(text=None)
    abstract = abstract.lstrip()
    abstract = abstract.strip('\n')
    abstract = '<section class="prog__container">%s</section>' % abstract
    return create_article(title, abstract, url)


with open('output.txt', 'w') as fp:
    for html_file in glob('download/*.html'):
        print('Processing %s' % html_file)
        soup = BeautifulSoup(open(html_file), 'html.parser')
        page_url = soup.find('link', attrs={'rel': 'canonical'}).get('href')
        print('Page url %s' % page_url)
        if 'api' in page_url:
            dls = soup.findAll('dl')
            for dl in dls:
                data = parse_dl(dl, page_url)
            print(page_url)
        else:
            h2s = soup.findAll('h2')
            for h2 in h2s:
                data = parse_h2(h2.parent, page_url)
                fp.write('{}\n'.format(data))
