#!/usr/bin env python
# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from glob import glob
from urllib.parse import urljoin


SEEN_TITLES = []
SEEN_REDIRECTS = []


def is_duplicate_entry(entry):
    if entry in SEEN_TITLES:
        return True
    if entry in SEEN_REDIRECTS:
        return True
    return False


def clean_title(title):
    for unwanted_text in ['¶', '[source]', '= None']:
        title = title.replace(unwanted_text, '')
    return title


def create_article(title, abstract, url):
    print('TITLE   : %s ' % title)
    print('URL     : %s ' % url)
    print('ABSTRACT: %s\n' % abstract)
    if title not in SEEN_TITLES:
        SEEN_TITLES.append(title)
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
    print('REDIRECT: {0} ~> {1}\n'.format(redirect_title, original_title))
    SEEN_REDIRECTS.append(redirect_title)
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


def create_redirect_titles(func_with_params='', func_without_params=''):
    # Superclass.class.method becomes [Superclass, class, method]
    fully_qualified_name_parts = func_without_params.split('.')

    # [Superclass, class, method] becomes method
    isolated_func_name = fully_qualified_name_parts[-1]

    # [Superclass, class, method] becomes Superclass class method
    module_func = ' '.join(fully_qualified_name_parts)

    # some_long_method becomes some long method
    spaced_func_without_params = ' '.join(isolated_func_name.split('_'))
    possible_redirect_titles = [module_func, func_without_params,
                                isolated_func_name,
                                spaced_func_without_params]
    redirect_titles = [i for i in set(possible_redirect_titles) if i and
                       i is not func_with_params]
    return redirect_titles


def format_abstract(abstract, code=None):
    abstract = '<p>{}</p>'.format(abstract)
    section = '<section class="prog__container">{0}{1}</section>'
    if code:
        code = '<pre><code>{}</code></pre>'.format(code)
        return section.format(abstract, code)
    return section.format(abstract, '')


def parse_dl(dl, page_url):
    '''Parse dl containing function definitions for requests api.

    Accepts a dl and page_url.
    Returns list of articles and or redirect entries
    '''
    output_data = []
    dt = dl.find('dt')
    func_with_params = clean_title(dt.text.strip())
    abstract = ''
    func_without_params = dt.get('id').strip()
    if '→' in func_with_params:
        # see /en/master/api/#requests.cookies.RequestsCookieJar.pop
        func_with_params, abstract = func_with_params.split('→')
        func_with_params = func_with_params.strip()
        abstract = '→ {} '.format(abstract)
    permalink = urljoin(page_url, '#{}'.format(func_without_params))
    redirect_titles = create_redirect_titles(func_with_params,
                                             func_without_params)
    dd = dl.find('dd')
    if dd.p:
        abstract += dd.p.text
        next_sibling = dd.p.find_next_sibling(text=None)
        while next_sibling and next_sibling.name == 'p':
            abstract += next_sibling.text
            next_sibling = next_sibling.find_next_sibling(text=None)
        table_params = dl.find('table')
        if table_params:
            for n, tr in enumerate(table_params.findAll('tr')):
                if n == 0:
                    abstract += '\\n<b>{}</b>'.format(tr.th.text)
                else:
                    abstract += '<b>{}</b>'.format(tr.th.text)
                if tr.td.findAll('li'):
                    abstract += '<ul>'
                    for li in tr.td.findAll('li'):
                        li_text = li.text.strip()
                        param, desc = li_text.split('--')
                        param, desc = param.strip(), desc.strip()
                        abstract += '<li><b>{0}</b> -- {1}</li>'.format(param,
                                                                        desc)
                    abstract += '</ul>'
                else:
                    if 'Return type' in tr.th.text:
                        # return type is last one, no need for space after it
                        abstract += ' ' + tr.td.text.strip()
                    elif 'Returns' in tr.th.text:
                        abstract += ' {}\\n'.format(tr.td.text.strip())
                    else:
                        abstract += ' ' + tr.td.text.strip()

        abstract = abstract.replace('\n', ' ')
    code = ''
    if dl.pre:
        code = dl.pre.text.replace('\n', '\\n')
    if abstract:
        abstract = format_abstract(abstract, code)
        out = create_article(func_with_params, abstract, permalink)
        output_data.append(out)
        for redirect_title in redirect_titles:
            if not is_duplicate_entry(redirect_title):
                redirect = create_redirect(redirect_title,
                                           func_with_params)
                output_data.append(redirect)
    output_data = [d for d in output_data if d]
    return output_data


def parse_h2(h2_parent, page_url):
    '''Extracts article details from h2.

    Accepts h2_parent and extracts title, url, abstract and code snippets.
    Returns a list
    '''
    h2 = h2_parent.find('h2')
    title = clean_title(h2.text)
    fragment = h2_parent.find('a').get('href')
    url = urljoin(page_url, fragment)
    abstract = ''
    code = ''
    next_sibling = h2.find_next_sibling(text=None)
    while next_sibling:
        if next_sibling.name == 'p':
            next_sibling_text = next_sibling.text.replace('\n', ' ')
            abstract += '{}'.format(next_sibling_text)
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
                code += pre_text.replace('\n', '\\n')
        next_sibling = next_sibling.find_next_sibling(text=None)
    abstract = abstract.lstrip()
    abstract = abstract.strip('\n')
    abstract = format_abstract(abstract, code)
    if title not in SEEN_TITLES:
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
                for dat in data:
                    fp.write('{}\n'.format(dat))
        else:
            h2s = soup.findAll('h2')
            for h2 in h2s:
                data = parse_h2(h2.parent, page_url)
                if data:
                    fp.write('{}\n'.format(data))
