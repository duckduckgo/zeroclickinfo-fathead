#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import csv, logging, os


class AlexaDomainItem:

  def __init__(self,domain,ranking):
    self.domain = domain
    self.ranking = ranking

  def __str__(self):
    fields = [ "Alexa ranking of %s" % (self.domain),
               "A",
               "",
               "",
               "",
               "",
               "",
               "",
               "[http://www.alexa.com/topsites Alexa Rankings]",
               "",
               "",
               "Alexa ranking of %s: %d" % (self.domain, self.ranking),
               "http://www.alexa.com/siteinfo/%s" % (self.domain) ]
    return "%s\n" % ("\t".join(fields))


class AlexaParser:

  CSV_FILE = "top-1m.csv"
  DOMAIN_COUNT = 1000000

  def __init__(self):
    self.csv_file = open(os.path.join("download",self.CSV_FILE),"rt")

  def __del__(self):
    self.csv_file.close()

  def __iter__(self):
    # construct csv parser
    self.csv_reader = csv.reader(self.csv_file,delimiter=",",quoting=csv.QUOTE_NONE)
    return self.__next__()

  def __next__(self):
    logger = logging.getLogger()

    # parse csv
    for line in self.csv_reader:
      ranking = int(line[0])
      domain = line[1]
      logger.debug("Got ranking for domain '%s'" % (domain) )
      yield AlexaDomainItem(domain,ranking)

    raise StopIteration


if __name__ == "__main__":

  # setup logger
  logging.basicConfig(level=logging.INFO,format="%(message)s")
  logger = logging.getLogger()

  # dump rankings
  parser = AlexaParser()
  count = 0
  with open("output.txt","wt") as output_file:
    for domain_item in parser:
      if ( count % (AlexaParser.DOMAIN_COUNT//100) ) == 0:
        progress = count // (AlexaParser.DOMAIN_COUNT//100)
        logger.info("%d%%" % (progress) )
      count += 1
      output_file.write(str(domain_item))
  logger.info("Parsed %d domain rankings successfully" % (count))
