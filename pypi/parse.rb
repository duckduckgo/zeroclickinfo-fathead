#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'rubygems'
require 'hpricot'
require 'open-uri'

def get_item(doc,label='Author:', child_tag="span")

   (doc/"//ul[@class='nodot']/li/strong[text()='#{label}']../#{child_tag}").inner_text
end

doc=Hpricot(open('download/pypy.html'))
apps = doc/"//table[@class='list']/tr"


apps.each do |l|

    if (l/'/td').size==2 # Its not the table header
  
      categories     = ''
      internal_links = ''
      external_links = 'http://pypi.python.org'+(l/"/td[1]/a").attr('href')
      images         = ''
    
      abstract       = (l/'/td[2]').inner_text 
      source_url     = external_links
      tmp           = (l/'/td[1]').inner_text  
      a=tmp.split(' ')
      page = a[0]

      
      abstract = "#{abstract}" unless a[1].nil? 
      abstract += '.' unless abstract=~/\.$/ 
      
      # Get the License and Home Page of the project from the detail page if available
      detail_doc=Hpricot(open(source_url))
      
      license = get_item(detail_doc,'License:')
      abstract += "  License: #{license}." unless license.nil? or license.strip==''
      
      official_site=get_item(detail_doc, 'Home Page:', 'a')
      abstract += "  <a href='#{official_site}'>Official Site</a>" unless official_site.nil? or official_site.strip==''
      
      unless abstract.nil?
        abstract.gsub!("\t", ' ') 
        abstract.gsub!("\n", ' ') 
        abstract.gsub!("\r", ' ') 
      end
    
#      puts "#{page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{images}\t#{abstract}\t#{source_url}\n"
      # Use programming format.
      puts "#{page}\t\t#{source_url}\t#{abstract}\t\t\t\t\n"
    
    end
    
end
