#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import re
from bs4 import BeautifulSoup


BASE_URL    = "http://en.cppreference.com/"
OUTPUL_FILE = open('output.txt', 'w+')
MODULE_DICT = {}
# URLS for each library is provided rather than scraping main pages for links
RESULT_URL  = ["http://en.cppreference.com/w/cpp/header/cstdlib", "http://en.cppreference.com/w/cpp/header/csignal", "http://en.cppreference.com/w/cpp/header/csetjmp",
			   "http://en.cppreference.com/w/cpp/header/cstdarg", "http://en.cppreference.com/w/cpp/header/typeinfo", "http://en.cppreference.com/w/cpp/header/typeindex",
			   "http://en.cppreference.com/w/cpp/header/type_traits", "http://en.cppreference.com/w/cpp/header/bitset", "http://en.cppreference.com/w/cpp/header/functional",
			   "http://en.cppreference.com/w/cpp/header/utility", "http://en.cppreference.com/w/cpp/header/ctime", "http://en.cppreference.com/w/cpp/header/chrono",
			   "http://en.cppreference.com/w/cpp/header/cstddef", "http://en.cppreference.com/w/cpp/header/initializer_list", "http://en.cppreference.com/w/cpp/header/tuple",
			   "http://en.cppreference.com/w/cpp/header/any", "http://en.cppreference.com/w/cpp/header/optional", "http://en.cppreference.com/w/cpp/header/variant",
			   "http://en.cppreference.com/w/cpp/header/new", "http://en.cppreference.com/w/cpp/header/memory", "http://en.cppreference.com/w/cpp/header/scoped_allocator",
			   "http://en.cppreference.com/w/cpp/header/memory_resource", "http://en.cppreference.com/w/cpp/header/climits", "http://en.cppreference.com/w/cpp/header/cfloat",
			   "http://en.cppreference.com/w/cpp/header/cstdint", "http://en.cppreference.com/w/cpp/header/cinttypes", "http://en.cppreference.com/w/cpp/header/limits",
			   "http://en.cppreference.com/w/cpp/header/exception", "http://en.cppreference.com/w/cpp/header/stdexcept", "http://en.cppreference.com/w/cpp/header/cassert",
			   "http://en.cppreference.com/w/cpp/header/system_error", "http://en.cppreference.com/w/cpp/header/cerrno", "http://en.cppreference.com/w/cpp/header/array", 
			   "http://en.cppreference.com/w/cpp/header/vector", "http://en.cppreference.com/w/cpp/header/deque", "http://en.cppreference.com/w/cpp/header/list",
			   "http://en.cppreference.com/w/cpp/header/forward_list", "http://en.cppreference.com/w/cpp/header/set", "http://en.cppreference.com/w/cpp/header/map",
			   "http://en.cppreference.com/w/cpp/header/unordered_set", "http://en.cppreference.com/w/cpp/header/unordered_map", "http://en.cppreference.com/w/cpp/header/stack",
			   "http://en.cppreference.com/w/cpp/header/queue", "http://en.cppreference.com/w/cpp/header/algorithm", "http://en.cppreference.com/w/cpp/header/execution",
			   "http://en.cppreference.com/w/cpp/header/iterator", "http://en.cppreference.com/w/cpp/header/cmath", "http://en.cppreference.com/w/cpp/header/complex",
			   "http://en.cppreference.com/w/cpp/header/valarray", "http://en.cppreference.com/w/cpp/header/random", "http://en.cppreference.com/w/cpp/header/numeric",
			   "http://en.cppreference.com/w/cpp/header/ratio", "http://en.cppreference.com/w/cpp/header/cfenv", "http://en.cppreference.com/w/cpp/header/iosfwd",
			   "http://en.cppreference.com/w/cpp/header/ios", "http://en.cppreference.com/w/cpp/header/istream", "http://en.cppreference.com/w/cpp/header/ostream",
			   "http://en.cppreference.com/w/cpp/header/iostream", "http://en.cppreference.com/w/cpp/header/fstream", "http://en.cppreference.com/w/cpp/header/sstream",
			   "http://en.cppreference.com/w/cpp/header/strstream", "http://en.cppreference.com/w/cpp/header/iomanip", "http://en.cppreference.com/w/cpp/header/streambuf",
			   "http://en.cppreference.com/w/cpp/header/cstdio", "http://en.cppreference.com/w/cpp/header/locale", "http://en.cppreference.com/w/cpp/header/clocale",
			   "http://en.cppreference.com/w/cpp/header/codecvt", "http://en.cppreference.com/w/cpp/header/regex", "http://en.cppreference.com/w/cpp/header/atomic",
			   "http://en.cppreference.com/w/cpp/header/thread", "http://en.cppreference.com/w/cpp/header/mutex", "http://en.cppreference.com/w/cpp/header/shared_mutex",
			   "http://en.cppreference.com/w/cpp/header/future", "http://en.cppreference.com/w/cpp/header/condition_variable", "http://en.cppreference.com/w/cpp/header/filesystem",
			   "http://en.cppreference.com/w/cpp/io/basic_streambuf", "http://en.cppreference.com/w/cpp/io/basic_filebuf", "http://en.cppreference.com/w/cpp/io/basic_stringbuf",
			   "http://en.cppreference.com/w/cpp/io/ios_base", "http://en.cppreference.com/w/cpp/io/basic_ios", "http://en.cppreference.com/w/cpp/io/basic_istream",
			   "http://en.cppreference.com/w/cpp/io/basic_ostream", "http://en.cppreference.com/w/cpp/io/basic_iostream", "http://en.cppreference.com/w/cpp/io/basic_ifstream",
			   "http://en.cppreference.com/w/cpp/io/basic_ofstream", "http://en.cppreference.com/w/cpp/io/basic_fstream", "http://en.cppreference.com/w/cpp/io/basic_istringstream",
			   "http://en.cppreference.com/w/cpp/io/basic_ostringstream", "http://en.cppreference.com/w/cpp/io/basic_stringstream", "http://en.cppreference.com/w/cpp/io/manip",
			   "http://en.cppreference.com/w/cpp/io/c", "http://en.cppreference.com/w/cpp/string/basic_string", "http://en.cppreference.com/w/cpp/string/basic_string_view",
			   "http://en.cppreference.com/w/cpp/string/byte", "http://en.cppreference.com/w/cpp/string/multibyte", "http://en.cppreference.com/w/cpp/string/wide",
			   "http://en.cppreference.com/w/cpp/container/array", "http://en.cppreference.com/w/cpp/container/vector", "http://en.cppreference.com/w/cpp/container/deque",
			   "http://en.cppreference.com/w/cpp/container/list", "http://en.cppreference.com/w/cpp/container/forward_list", "http://en.cppreference.com/w/cpp/container/set",
			   "http://en.cppreference.com/w/cpp/container/multiset", "http://en.cppreference.com/w/cpp/container/map", "http://en.cppreference.com/w/cpp/container/multimap",
			   "http://en.cppreference.com/w/cpp/container/unordered_set", "http://en.cppreference.com/w/cpp/container/unordered_multiset", "http://en.cppreference.com/w/cpp/container/unordered_map",
			   "http://en.cppreference.com/w/cpp/container/unordered_multimap", "http://en.cppreference.com/w/cpp/container/stack", "http://en.cppreference.com/w/cpp/container/queue",
			   "http://en.cppreference.com/w/cpp/container/priority_queue"]

def print_line(title, abstract):
    
    # Duplicate check
	if(MODULE_DICT.has_key(title)):
		return

	MODULE_DICT[title] = 1

	abstract = abstract.replace('\n','\\n')
	abstract = abstract.replace('\t', '  ')
	abstract = abstract.replace(u'\u2019','&#8217;')
	abstract = abstract.replace(u'\u201c','&#8220;')
	abstract = abstract.replace(u'\u201d','&#8221;')
	abstract = abstract.replace(u'\xb6','')
	abstract = abstract.replace(u'\xa0','')
	abstract = abstract.replace(u'\u2013','&#8211;')
	abstract = abstract.replace(u'\u2018','&#8216;')

	abstract = '<section class="prog__container"><pre><code>' + abstract + '</code></pre></section>'

	list_of_data = [
	    title,      # 1. article title
	    'A',        # 2.type is article
	    '',         # 3.no redirect data
	    '',         # 4.leave empty
	    '',         # 5.no categories
	    '',         # 6.leave empty
	    '',         # 7.no related topics
	    '',         # 8.leave empty
	    '',    		# 9.an external link back to home
	    '',         # 10.no disambiguation
	    '',         # 11.images
	    abstract,   # 12.abstract
	    BASE_URL    # 13.url to doc
	]

	OUTPUL_FILE.write('{}\n'.format('\t'.join(list_of_data)))

# Fetch example code, if exists
def get_example_code(title, url):

	page     = requests.get(url)
	src      = page.text
	ob       = BeautifulSoup(src, 'html.parser')

	print_line(title + ' ' + url.split('/')[-1], ob.find('div',{'class':'t-example'}).text)	

# Fetch the URL of a particular function
def _get_func_name(url):

	page     = requests.get(url)
	src      = page.text
	idx      = src.rfind("Functions") # Used for getting links to only functions
	if idx==-1:
		idx  = src.rfind("Member functions") # Some functions are listed as Member Functions
	src      = src[idx:]
	ob       = BeautifulSoup(src, 'html.parser')

	for info in ob.findAll('tr',{'class':'t-dsc'}):
		try:
			get_example_code(url.split('/')[-1], BASE_URL+info.find('td').find('a')['href'])
		except:
			continue 	

def get_func_name():

	for url in RESULT_URL:
		_get_func_name(url)

if __name__=="__main__":

    get_func_name() # Extract all links to the functions