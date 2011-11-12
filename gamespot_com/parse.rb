#!/usr/bin/ruby
['rubygems', 'hpricot', 'open-uri'].each{|r|require r}

DOWNLOAD_DIR='download'

# Have logic related to previous record (to aggregate game systems for a game)
@game_system      = []
@external_links   = []
@prev_page        = nil
@images           = ''
@text_abstract    = ''
@page             = ''
@source_url       = ''

def parse_file(file, output_file)
   
   doc=Hpricot(open(file))
   apps = doc/"//a"
   apps.each do |l|

   internal_links  = ''
   categories      = ''

   tmp            = l.inner_text.split('-')
   # Aggregating games by name.  External links by system
   if (not @prev_page.nil?) and tmp[0].strip.downcase != @prev_page.downcase 
      external_links='' # Aggregate the external links into a string
      @external_links.each_with_index{|link,i|external_links+="[#{link} #{@game_system[i]}]\\n"}
       
      str="#{@page}\tA\t\t\t#{categories}\t\t#{internal_links}\t\t#{external_links}\t\t#{@images}\t#{@text_abstract}\t#{@source_url}\n"
      File.open(output_file, 'a'){|f|f.puts str}
     
      @images         = ''
      @text_abstract  = ''  
      @source_url     = ''
      @page           = ''
      @game_system    = []
      @external_links = []
       
   end

    @page           = tmp[0].strip
    @game_system    << tmp[1].strip
    @prev_page      = @page
    @source_url     = l['href']
    @external_links << @source_url
    
  end

end

parse_file(ARGV[0], ARGV[1])
