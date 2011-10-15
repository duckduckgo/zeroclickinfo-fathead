#!/usr/bin/ruby

require 'rubygems'
require 'hpricot'
require 'open-uri'

doc=Hpricot(open('download/pypy.html'))
apps = doc/"//table[@class='list']/tr"


apps.each do |l|

    if (l/'/td').size==2 # Its not the table header
  
      categories     = 'python, python package'
      internal_links = ''
      external_links = 'http://pypi.python.org/pypi'+(l/"/td[1]/a").attr('href')
      images         = ''
    
      abstract       = (l/'/td[2]').inner_text 
      source_url     = external_links
      tmp           = (l/'/td[1]').inner_text  
      a=tmp.split(' ')
      page = a[0]

      
      abstract = "(version #{a[1]}) #{abstract}" unless a[1].nil? 

      unless abstract.nil?
        abstract.gsub!("\t", ' ') 
        abstract.gsub!("\n", ' ') 
        abstract.gsub!("\r", ' ') 
      end
    
      puts "#{page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{images}\t#{abstract}\t#{source_url}\n"
    
    end
    
end