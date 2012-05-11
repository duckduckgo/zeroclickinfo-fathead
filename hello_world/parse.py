#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import csv, logging, os, glob


class HelloWorldItem:
  def __init__(self, language, filename, source):
    self.language = language
    self.filename = filename
    self.source = source

  def __str__(self):
    # a few problems:
     # 1. self.source may contain tabs
     # 2. should type or lang be non-empty?
    fields = [ "hello world (%s)" % self.language,
               "", # namespace
               "https://github.com/leachim6/hello-world",
               "Hello World in %s (%s)" % (self.language, self.filename),
               self.source, # synposis (code)
               "", # details
               "", # type
               "", # lang
             ]

    output = "%s\n" % ("\t".join(fields))
    fields[0] = '%s hello world' % self.language
    output += "%s\n" % ("\t".join(fields))
    fields[0] = 'hello world in %s' % self.language
    output += "%s\n" % ("\t".join(fields))
    return output


if __name__ == "__main__":
    # setup logger
    logging.basicConfig(level=logging.INFO,format="%(message)s")
    logger = logging.getLogger()
    
    # dump config items
    count = 0
    with open("output.txt", "wt") as output_file:
        for filepath in glob.glob('download/*/*'):
            _,filename = os.path.split(filepath)
            # ignore some "languages"
            if filename not in ['ls.ls', 'readlink.readlink', 'piet.png']:
                # fix brainfuck name
                if filename == 'brainf*ck.bf':
                    filename = 'brainfuck.bf'

                language,_ = os.path.splitext(filename)
                with open(filepath, 'r') as f:
                    source = f.read()
                source = source.replace('\\n', '~~~n')
                source = source.replace('\n', '\\n')
                source = source.replace('~~~n', '\\\\n')
                source = source.replace('\t', '\\t')
                    
                item = HelloWorldItem(language, filename, source)
                if count % 10 == 0:
                    logger.info("%d languages processed" % count )

                count += 1
                output_file.write(str(item))
    logger.info("Parsed %d domain rankings successfully" % count)

