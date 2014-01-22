from BeautifulSoup import BeautifulSoup
import re
import os
import sys


openclosetags = re.compile('''<.*?>|</.*?>''',re.DOTALL)

files = []

files.append('./docs/emacs.txt')

for file in files:
  for line in open(file):
    for command in line.split("\t"):
      if command.strip() != '':
        desc = command.strip().split(' ')[-1:][0]
        synopsis = ' '.join(command.strip().split(' ')[:-1])
        name = command.strip()
        url = ''
        namespace = ''

        if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
          print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(name,'',url,desc.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','emacs','en')
        if sys.argv[1].lower() == 'sql':
          print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(name.replace("'","''"),namespace,url,desc.replace("'","''"),synopsis.replace("'","''"),'','emacs','')
