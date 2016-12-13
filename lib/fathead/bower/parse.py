#!/usr/bin/env python3

import sys
import re

URL_BASE = 'https://bower.io/docs/api/#'
CODE_PATTERN = re.compile(
    r'{% highlight sh %}(.*){% endhighlight %}',
    re.DOTALL
)

items = []


def add_item(ops):
    # Write articles
    items.append(
     '\t'.join([
        ops['name'],
        'A',
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        ops['abstract'],
        URL_BASE + ops['URL']
        ]))
    # Write 'command' redirect for each article
    items.append(
     '\t'.join([
        '%s command' % ops['name'],
        'R',
        ops['name'],
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        '',
        ''
        ]))

def parse_item(text):
    # remove all child block
    text = text.split('\n####')[0]
    name = text.split('\n')[0].strip()
    code = re.search(CODE_PATTERN, text)
    code = '<pre><code>%s</code></pre>' % (code.group(1)) if code else ''
    desc = "".join(text.split(r'{% endhighlight %}')[1:]).strip()
    abstract = ('<section class="prog__container">' + desc + code + '</section>').replace('\n', '\\n',).replace('\t', '  ')
    return {
        'name': 'api ' + name,
        'abstract': abstract,
        'URL': name
    }

api_doc = ""

with open('download/api.md', 'r') as f:
    api_doc = f.read()

if not api_doc:
    sys.exit(-1)

api_doc_blocks = []

# only parse `Command` and `Options` block
for h2_block in api_doc.split('\n## ')[1:3]:
    api_doc_blocks.extend(h2_block.split('\n### ')[1:])
for block in api_doc_blocks:
    add_item(parse_item(block))

with open('output.txt', 'w') as f:
    f.write("\n".join(items))
