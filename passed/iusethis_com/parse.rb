#!/usr/bin/ruby

# require 'rubygems'
require 'hpricot'
require 'open-uri'

DOWNLOAD_DIR='download'

@title_list=[]

def parse_file(file)

  doc=Hpricot(open(file))

  apps = doc/"//div[@id='maincontent']/div[@class='individualapps']"    

  apps.each do |l|
    os=file.split('_')[1]
    categories     = os
    
    # Adding the first tag entry to the category as more descriptive
    tags=(l/"//p[@class='applinks']/a[3]/b").inner_text
    categories+=" #{tags}"
    
    internal_links = ''
    external_links = (l/"/div[@class='iconandcount']/div[@class='iconside']/a").attr('href')
    images         = (l/"/div[@class='iconandcount']/div[@class='iconside']/a/img").attr('src')
    
    abstract       = (l/"/div[@class='appcontent']/..//div[@class='description']/p").inner_text  
    source_url     = (l/"/div[@class='appcontent']/h2/a").attr('href') 
    page           = (l/"/div[@class='appcontent']/h2/a").inner_text  
    
    # Prevent dups.  Because of current state of data, os is always Windows at the time this is written
    page += " for #{os.capitalize}" if @title_list.include?(page)
    
    @title_list << page
    
    unless abstract.nil?
      abstract.gsub!("\t", ' ') 
      abstract.gsub!("\n", ' ') 
      abstract.gsub!("\r", ' ') 
    end
    
    puts "#{page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{images}\t#{abstract}\t#{source_url}\n"
  end

end

#
# Process all html files in the download directory
#
Dir.new(DOWNLOAD_DIR).entries.sort.each do|file|
  parse_file(DOWNLOAD_DIR+'/'+file) if file=~/\.html$/
end
