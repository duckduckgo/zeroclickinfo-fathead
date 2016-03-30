import re
import os
import sys

files = []

files.append('./docs/linuxkernelerror/errorno.h')

for file in files:
  filecontents = ''
  for line in open(file):
    line = line.strip()

    t = line.split("\t")
    if len(t) == 5:
      searchname = t[1]
      url = 'http://sysinf0.klabs.be/usr/include/asm-generic/errno.h'
      name = "%s - %s"%(t[3],t[4].replace('/* ','').replace(' */',''))
      synopsis = ''
      description = ''

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(searchname,'',url,name.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','linuxkernelheader','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(searchname,'',url,name,synopsis,description,'linuxkernelerror','')

files = []
files.append('./docs/linuxkernelerror/errorno-base.h')


for file in files:
  filecontents = ''
  for line in open(file):
    line = line.strip()

    t = line.split("\t")
    if len(t) == 5:
      searchname = t[1]
      url = 'http://sysinf0.klabs.be/usr/include/asm-generic/errno-base.h'
      name = "%s - %s"%(t[3],t[4].replace('/* ','').replace(' */',''))
      synopsis = ''
      description = ''

      if len(sys.argv) == 1 or sys.argv[1].lower() == 'tsv':
        print "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"%(searchname,'',url,name.replace("\r\n","__NEWLINE__"),synopsis.replace("\r\n","__NEWLINE__"),'','linuxkernelheader','en')
      if sys.argv[1].lower() == 'sql':
        print '''INSERT INTO functions (`id`, `name`, `namespace`, `url`, `description`, `synopsis`, `detail`, `type`, `lang`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');'''%(searchname,'',url,name,synopsis,description,'linuxkernelerror','')
