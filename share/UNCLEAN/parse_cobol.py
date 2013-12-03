from BeautifulSoup import BeautifulSoup
import re
import os
import sys
import string

openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)
spaces = re.compile('''\s+''',re.DOTALL)

files = []

files.append('./docs/cobol.htm')

args = {
	'ABS':'( argument-1 )',
	'ACOS':'( argument-1 )',
	'ANNUITY':'( argument-1 argument-2 )',
	'ASIN':'( argument-1 )',
	'ATAN':'( argument-1 )',
	'CHAR':'( argument-1 )',
	'CHAR-NATIONAL':'( argument-1 )',
	'COS':'( argument-1 )',
	'CURRENT-DATE':'',
	'DATE-OF-INTEGER':'( argument-1 )',
	'DATE-TO-YYYYMMDD':'( argument-1 [argument-2] )',
	'DAY-OF-INTEGER':'( argument-1 )',
	'DAY-TO-YYYYDDD':'( argument-1 [argument-2] )',
	'DISPLAY-OF':'( argument-1 [argument-2] )',
	'E':'',
	'EXP':'( argument-1 )',
	'EXP10':'( argument-1 )',
	'FACTORIAL':'( argument-1 )',
	'FRACTION-PART':'( argument-1 )',
	'INTEGER':'( argument-1 )',
	'INTEGER-OF-DATE':'( argument-1 )',
	'INTEGER-OF-DAY':'( argument-1 )',
	'INTEGER-PART':'( argument-1 )',
	'LENGTH':'( argument-1 )',
	'LENGTH-AN':'( argument-1 )',
	'LOG':'( argument-1 )',
	'LOG10':'( argument-1 )',
	'LOWER-CASE':'( argument-1 )',
	'MAX':'( argument-1 )',
	'MEAN':'( { argument-1 } ... )',
	'MEDIAN':'( { argument-1 } ... )',
	'MIDRANGE':'( { argument-1 } ... )',
	'MIN':'( { argument-1 } ... )',
	'MOD':'( argument-1 argument-2 )',
	'NATIONAL-OF':'( argument-1 [argument-2] )',
	'NUMVAL':'( argument-1 )',
	'NUMVAL-C':'( argument-1 [argument-2] )',
	'ORD':'( argument-1 )',
	'ORD-MAX':'( { argument-1 } ... )',
	'ORD-MIN':'( { argument-1 } ... )',
	'PI':'',
	'PRESENT-VALUE':'( argument-1 [argument-2] )',
	'RANDOM':'[ ( argument-1 ) ]',
	'RANGE':'( { argument-1 } ... )',
	'REM':'( argument-1 argument-2 )',
	'REVERSE':'( argument-1 )',
	'SIGN':'( argument-1 )',
	'SIN':'( argument-1 )',
	'SQRT':'( argument-1 )',
	'STANDARD-DEVIATION':'( { argument-1 } ... )',
	'SUM':'( { argument-1 } ... )',
	'TAN':'( argument-1 )',
	'UPPER-CASE':'( argument-1 )',
	'VARIANCE':'( { argument-1 } ... )',
	'WHEN-COMPILED':'',
	'YEAR-TO-YYYY':'( argument-1 [argument-2] ) ',
}


for file in files:
	filecontents = open(file).read()
	soup = BeautifulSoup(filecontents)
	for s in soup.findAll('h3'):
		t = re.compile('''[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{1,2} ''',re.DOTALL)
		name = t.sub('',str(s))
		name = openclosetags.sub('',name.replace('The ','').replace(' Function','').replace(' function',''))
		
		desc =  str(s.nextSibling.nextSibling)
		if "dialm.gif" in desc:
			desc = str(s.nextSibling.nextSibling.nextSibling.nextSibling)
		desc = openclosetags.sub('',desc)
		
		url = "http://supportline.microfocus.com/documentation/books/sx20books/lrpdf7.htm#%s"%s.findAll('a')[0]['name']
		
		
		synopsis = "FUNCTION %s %s"%(name,args[name.strip()])
		
		
		if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
			print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc.replace("\n","__NEWLINE__"),synopsis.replace("\n","__NEWLINE__"),'','cobol','en')
		if sys.argv[1].lower() == 'sql':
			print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name,'',url,desc,synopsis,'','cobol','en')
		
		