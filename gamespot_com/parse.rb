#!/usr/bin/ruby

require 'rubygems'
require 'hpricot'
require 'open-uri'
require 'find'

DOWNLOAD_DIR='download'

def parse_file(file)

  doc=Hpricot(open(file))
  apps = doc/"//ul[@class='games']/li"
  apps.each do |l|
    
    tmp = (l/'//h3/a')
    source_url     = tmp.attr('href') 
    external_links = source_url
    page           = tmp.inner_text.strip
    
    file_info = file.split('/').last.split('_')
    
    categories     = file_info[0] # Game System
    internal_links = ''
    
    images=""
    images         = (l/"//div[@class='thumb']/a/img").attr('src') rescue ""
    
    text_abstract  = (l/"//div[@class='deck']//p").inner_text

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
