#!/usr/bin/ruby

require 'rubygems'
require 'hpricot'
require 'open-uri'

DOWNLOAD_DIR='download'

def parse_file(file)

  doc=Hpricot(open(file))

  apps = doc/"//div[@class='appinfo']"

  apps.each do |l|

    categories     = ''
    internal_links = ''
    external_links = ''
    source_url     = (l/'/a').attr('href')

    id=(l/'/a').attr('id').split('-')[1]
    
    images        = "http://s2.macupdate.com/util/iconlg/#{id}.png"
    
    # Strip the version number
    tmp = (l/'/a').inner_text

    # ...first remove references to beta
    tmp.gsub!(/( Public)? ([B|b]eta( [0-9])?|Beta)$/,'')
    
    # ... then strip the version number from the end (any text following the last space)
    last_space=tmp.rindex(' ')
    page          = tmp[0...last_space]
    
    abstract      = (l/'/span').inner_text.gsub(/^ - /,'')
    
    unless abstract.nil?
      abstract.gsub!("\t", ' ') 
      abstract.gsub!("\n", ' ') 
      abstract.gsub!("\r", ' ') 
    end
    # puts tmp
    puts "#{page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{images}\t#{abstract}\t#{source_url}\n"
  end

end

#
# Process all html files in the download directory
#
Dir.new(DOWNLOAD_DIR).entries.each do|file|
  parse_file(DOWNLOAD_DIR+'/'+file) if file=~/\.html$/
end
