#!/usr/bin/ruby
['rubygems','hpricot','open-uri','find'].each{|r|require r}

DOWNLOAD_DIR='download'
INDEX_PAGE='_index.html'
SCRIPT1='fetch_alpha_script.sh'
SCRIPT2='fetch_games_script.sh'
category_links=[]

def run(script); system "bash #{script}"; end
def curl_str(url, output); "curl \"#{url}\" --output \"#{output}\""; end

#
# Go through the Sitemap and get the categories
#
doc=Hpricot(open('http://sitemap.gamespot.com/'))
(doc/'//a').each{|x|category_links << x if x['href']=~/\/games\//}

File.open(SCRIPT1,'w')do|o|
  o.puts "mkdir #{DOWNLOAD_DIR}"
  category_links.each do |l|
    category_page=l['href']
    tags=l.inner_text.split('>')
    cat=tags.map{|l|l.strip!;l.gsub(/(\s|\'|\-|\/)/,'_')}.join('-')
    o.puts "mkdir download/#{cat}" 
    o.puts curl_str(category_page,"#{DOWNLOAD_DIR}/#{cat}_#{INDEX_PAGE}")
  end

end 

run(SCRIPT1)

#
# Games indexed by letter.  Get each link for a given category / letter
#
File.open(SCRIPT2,'w') do |o|
   Find.find(DOWNLOAD_DIR) do |f| 
     if f=~/\_index.html$/
        dir = f.gsub(INDEX_PAGE,'')
        o.puts "mkdir #{dir}"
        (Hpricot(open(f))/'//a').each do |a|
           file="#{a.inner_text.gsub('#','999')}.html"
           o.puts curl_str(a['href'],"#{dir}/#{file}")
         end
     end
  end
end

run(SCRIPT2)