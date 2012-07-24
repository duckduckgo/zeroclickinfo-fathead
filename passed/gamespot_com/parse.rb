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

def get_details(url,name, output_file)
  img=''
  abstract_text=''
  
  begin   
     tmp="tmp_"+output_file
     system "curl -L #{url} -o #{tmp}" 
     doc2=Hpricot(open(tmp))
     abstract_text = (doc2/"//span[@class='mainDeck']").inner_text
     img=(doc2/"//div[@class='boxshot']/a/img").first['src']     
  rescue
     puts "*** WARNING: Could not retrieve abstract / image for #{url}"
  end

  system "rm #{tmp}" rescue puts "WARNING: Could not remove #{tmp} (#{$!})"
  return [img,abstract_text]
end

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
      
      # Go get the detail information once we are down to one version of the game...
      @images,@text_abstract=get_details(@source_url, @page, output_file) 
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
