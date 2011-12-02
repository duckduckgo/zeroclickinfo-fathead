#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging, os, urllib.parse
import lxml.etree

# Python code used to generate the site is freely available (http://cateee.net/sources/lkddb/) 
# but it's a lot more maintanable to scrape their site than to hack around their kernel parsing code


class KernelConfigItem:

  def __init__(self,url,name,shorthelp,help,type,depends,defined,kernelversions,modules):
    self.url = url
    self.name = name
    self.shorthelp = shorthelp
    self.help = help
    self.type = type
    self.depends = depends
    self.defined = defined
    self.kernelversions = kernelversions
    self.modules = modules

  def __str__(self):
    redirect = "%s\tR\t%s\t\t\t\t\t\t\t\t\t\t" % (self.name.split("_",1)[1],self.name)
    if self.help:
      snippet = self.help
    """
    if self.type:
      snippet = "%s\\n- type: %s" % (snippet, self.type)
    if self.depends:
      snippet = "%s\\n- depends on the following option(s): %s" % (snippet, self.depends)
    if self.defined:
      snippet = "%s\\n- defined in: %s" % (snippet, self.defined)
    if self.kernelversions:
      snippet = "%s\\n- available in the following Linux version(s): %s" % (snippet, self.kernelversions)
    if self.modules:
      snippet = "%s\\n- will build the following module(s): %s" % (snippet, self.modules)
    """
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
               snippet,
               self.url ]
    return "%s\n%s\n" % (redirect, "\t".join(fields))


class LkddbScraper:

  BASE_URL = "http://cateee.net/lkddb/web-lkddb/"
  INDEX_URL = "%sindex.html" % (BASE_URL)

  parser = lxml.etree.HTMLParser()

  def __init__(self):
    self.ok_count = 0
    self.ko_count = 0

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
          # has this option several descriptions?
          multiple = len(config_page_xml.findall("body/div/ul")) > 3

          # get name of config option
          name = config_page_xml.find("body/div/h1").text
          if name.find(":") != -1:
            name = name.split(":",1)[0]

          # get full help
          help_lines = []
          for help_line in config_page_xml.xpath("body/div/*[self::h2 or self::h3][text()='Help text']/following-sibling::*"):
            if help_line.tag != "p":
              break
            help_lines.append(lxml.etree.tostring(help_line,encoding="unicode",method="text").replace("\n"," ").strip())
          help = "\\n".join(help_lines)
          if (not help) or (help == "(none)"):
            self.ko_count += 1
            logger.warning("Skipping page '%s' (not enough content)" % (config_page_url) )
            continue

          # detect erroneous pages
          pre_list_msg_xml = config_page_xml.xpath("body/div/h2[text()='General informations']/following::p")
          if not config_page_xml.xpath("body/div/h2[text()='General informations']/following::p"):
            # for some pages lxml fail to get the right xml structure
            # eg: http://cateee.net/lkddb/web-lkddb/M25PXX_USE_FAST_READ.html (xpath query "body/div/h2[text()='General informations']" gives nothing, but "body//h2[text()='General informations']" does)
            self.ko_count += 1
            logger.warning("Parsing of page '%s' failed (invalid xml structure)" % (config_page_url) )
            continue
          pre_list_msg_xml = pre_list_msg_xml[0]
          pre_list_msg = lxml.etree.tostring(pre_list_msg_xml,encoding="unicode",method="text").strip()
          if pre_list_msg.endswith("error: definition not found!"):
            # some pages are incomplete, eg: http://cateee.net/lkddb/web-lkddb/ATHEROS_AR71XX.html
            self.ko_count += 1
            logger.warning("Parsing of page '%s' failed (incomplete page)" % (config_page_url) )
            continue

          # get other option info
          if multiple:
            li_list = list(x.text for x in config_page_xml.xpath("body/div/h2[1]/following::ul/li") if x.text)
          else:
            li_list = list(x.text for x in config_page_xml.xpath("body/div/h2[text()='General informations']/following::ul/li") if x.text)
          shorthelp, type, depends, defined, kernelversions, modules = None, None, None, None, None, None
          for li in li_list:
            if li.startswith("prompt: "):
              shorthelp = li[8:]
            elif li.startswith("type: "):
              type = li[6:]
            elif li.startswith("depends on: "):
              depends = li[12:]
            elif li.startswith("defined in: "):
              defined = li[12:]
            elif li.startswith("found in Linux kernels: "):
              kernelversions = li[24:]
            elif li.startswith("modules built: "):
              modules = li[15:]

          self.ok_count += 1
          logger.info("Page '%s' parsed successfully" % (config_page_url) )
          yield KernelConfigItem(config_page_url,name,shorthelp,help,type,depends,defined,kernelversions,modules)

        except UnicodeDecodeError:
          # some pages contain badly encoded chars, eg: http://cateee.net/lkddb/web-lkddb/SA1100_PFS168.html
          self.ko_count += 1
          logger.warning("Parsing of page '%s' failed (invalid character)" % (config_page_url) )
        except:
          # unknown error
          self.ko_count += 1
          logger.warning("Parsing of page '%s' failed" % (config_page_url) )
          raise
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
  logger.setLevel(logging.INFO)
  
  # dump config items
  scraper = LkddbScraper()
  with open("output.txt","wt") as output_file:
    for config_item in scraper:
      output_file.write(str(config_item))
  logger.warning("%d config items parsed successfully\n%d skipped pages" % (scraper.ok_count, scraper.ko_count) )

