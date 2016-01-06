import glob

def parse_article(a, fn):
	l = [x for x in a.split('\n') if x]
	title = l[0].lstrip('# ')
	title += ' (' + l[1].lstrip('> ') + ')'
	desc = ''
	for i in l[2:]:
		if i.startswith('`'):
			desc += '<pre><code>{}</code></pre>\\n'.format(i.strip('`'))
		elif i.startswith('-'):
			desc += i.lstrip('- ') + '\\n'
	url = 'https://github.com/tldr-pages/tldr/blob/master' + fn
	return '\t'.join([title, 'A', '', '', '', '', '', '', '', '', '', desc.rstrip('\\n'), url])

for fn in glob.glob('download/pages/common/*.md'):
	print parse_article(open(fn).read(), fn.lstrip('download'))
