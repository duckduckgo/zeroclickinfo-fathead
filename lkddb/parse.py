#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging, os, urllib.parse
import lxml.etree

# Python code used to generate the site is freely available (http://cateee.net/sources/lkddb/) 
# but it's a lot more maintanable to scrape their site than to hack around their kernel parsing code


class KernelConfigItem:

  def __init__(self,url,name,shorthelp,help,type,depends,defined,kernelversions):
    self.url = url
    self.name = name
    self.shorthelp = shorthelp
    self.help = help
    self.type = type
    self.depends = depends
    self.defined = defined
    self.kernelversions = kernelversions

  def __str__(self):
    fields = [ self.name,
               "A",
               "",
               "",
               "",
               "",
               "",
               "",
               "[http://cateee.net/lkddb/web-lkddb/ Linux Kernel Driver DataBase]",
               "",
               "",
               self.help, # TODO format this nicely with other info
               self.url ]
    return "%s\n" % ("\t".join(fields))


class LkddbScraper:

  BASE_URL = "http://cateee.net/lkddb/web-lkddb/"
  INDEX_URL = "%sindex.html" % (BASE_URL)

  parser = lxml.etree.HTMLParser()

  def __iter__(self):
    # get main page
    main_page = self.get_page_from_cache(self.INDEX_URL)
    self.main_page_xml = lxml.etree.XML(main_page,self.parser)
    return self.__next__()

  def __next__(self):
    logger = logging.getLogger()
    # get subpages
    for sub_page_tag in self.main_page_xml.iterfind("body/ul/li/a"):
      sub_page_url = "%s%s" % (self.BASE_URL, sub_page_tag.attrib["href"])
      sub_page = self.get_page_from_cache(sub_page_url)
      sub_page_xml = lxml.etree.XML(sub_page,self.parser)
      # get config item page
      for config_page_tag in sub_page_xml.iterfind("body/ul/li/ul/li/a"):
        config_page_url = "%s%s" % (self.BASE_URL, config_page_tag.attrib["href"])
        config_page = self.get_page_from_cache(config_page_url)
        config_page_xml = lxml.etree.XML(config_page,self.parser)
        try:
          # construct config item
          multiple = len(config_page_xml.findall("body/div/ul")) > 3
          # get name of config option
          name = config_page_xml.find("body/div/h1").text
          if name.find(":") != -1:
            name = name.split(":")[0]
          if not multiple:
            # get short description
            li_list = list(x.text for x in config_page_xml.findall("body/div/ul/li"))
            shorthelp = li_list[0].split(": ")[1]
            # get full help
            help_lines = []
            for help_line in config_page_xml.xpath("body/div/h2[text()='Help text']/following::*"):
              # TODO some lines are lost, eg: "To compile this driver as a module..." on http://cateee.net/lkddb/web-lkddb/6PACK.html
              if help_line.tag != "p":
                break
              help_lines.append(lxml.etree.tostring(help_line,encoding="unicode",method="text").replace("\n"," ").strip())
            help = "\\n".join(help_lines)
            if help == "(none)":
              help = ""
            # get other option info
            type = li_list[1].split(": ")[1]
            depends = li_list[2].split(": ")[1]
            defined = li_list[3].split("in ")[1]
            kernelversions = li_list[4].split(": ")[1]
            # TODO get "modules built: xxx" line
          else:
            # TODO handle options with more than one description (lots!)
            continue
          yield KernelConfigItem(config_page_url,name,shorthelp,help,type,depends,defined,kernelversions)
        except:
          # some pages are incomplete, eg: http://cateee.net/lkddb/web-lkddb/ATHEROS_AR71XX.html
          # we give up for those pages (currently 18 on 9613 parsed pages)
          logger.warning("Parsing of page '%s' failed" % (config_page_url) )
          #raise
    raise StopIteration

  def get_page_from_cache(self,url):
    logger = logging.getLogger()
    # get path to local file
    domain = urllib.parse.urlparse(url)[1]
    page = urllib.parse.urlparse(url)[2]
    local_filepath = os.path.join("download",domain)
    for subdir in page.split("/")[1:]:
      local_filepath = os.path.join(local_filepath,subdir)
    logger.debug("Getting local file '%s' for url '%s'..." % (local_filepath,url) )
    # read file
    with open(local_filepath,"rb") as file:
      page = file.read()
    return page


if __name__ == '__main__':

  # setup logger
  logger = logging.getLogger()
  logger.setLevel(logging.DEBUG)
#  logger.setLevel(logging.INFO)
  #logger.setLevel(logging.WARNING)
  
  # dump config items
  scraper = LkddbScraper()
  count = 0
  with open("output.txt","wt") as output_file:
    for config_item in scraper:
      count += 1
      output_file.write(str(config_item))
  logger.info("%d config items" % (count) )

