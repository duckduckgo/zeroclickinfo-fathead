#!/usr/bin/ruby

require 'rubygems'
require 'hpricot'
require 'open-uri'
require 'find'

DOWNLOAD_DIR='download'


def parse_file(file)

  doc=Hpricot(open(file))
  apps = doc/"//ul[@class='downloads']/li"
  apps.each do |l|
    
    tmp = (l/'//h3/a')
    source_url     = tmp.attr('href') 
    external_links = ''
    internal_links = ''
    page           = tmp.inner_text.strip
    
    file_info = file.split('/')[1].gsub(/\_[09]*\.html$/,'')
    
    categories     = ''

    images=""
    images         = (l/"/div/a/img").attr('src') rescue ""
    
    text_abstract  = (l/"/div//p[@class['product-description']]").inner_text

    unless text_abstract.nil? 
      text_abstract.gsub!("\t", ' ') 
      text_abstract.gsub!("\n", ' ') 
      text_abstract.gsub!("\r", ' ') 
    end
    
    puts "#{page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{images}\t#{text_abstract}\t#{source_url}\n"
  end

end

#
# Process all html files in the download directory (recursive)
#
Find.find('download'){|f| parse_file(f) if f=~/\.html$/}

