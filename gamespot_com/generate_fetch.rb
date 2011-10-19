require 'rubygems'
require 'hpricot'
require 'open-uri'

DOWNLOAD_DIR='download'

doc=Hpricot(open('http://www.gamespot.com/games.html?platform=1029&mode=top&tag=masthead%3Bxbox360%3Ball_games'))

platforms=[]
categories=[]

(doc/"//select[@name='platform']/option").each{|x|platforms<< [x['value'], x.inner_text]    unless x.inner_text=~/\-\-\-/ or x.inner_text=='All Platforms'}
(doc/"//select[@name='category']/option").each{|x|categories << x.inner_text unless x.inner_text=~/\-\-\-/ or x.inner_text=='All Games'}

puts "mkdir -p #{DOWNLOAD_DIR}"

puts "mkdir -p #{DOWNLOAD_DIR}/iPhone"
puts "mkdir -p #{DOWNLOAD_DIR}/APF-1000"
puts "mkdir -p #{DOWNLOAD_DIR}/Online"
puts "mkdir -p #{DOWNLOAD_DIR}/OS"
puts "mkdir -p #{DOWNLOAD_DIR}/TI-99"
puts "mkdir -p #{DOWNLOAD_DIR}/Unix"


platforms.each do |p|
  	platform_id=p[0]
  	platform_name=p[1].gsub(' ','_').gsub('*','')

   categories.each do |category|
      category.gsub!(' ','+')
		puts "curl \"http://www.gamespot.com/games.html?platform=#{platform_id}&category=#{category}&letter=999\"   --limit-rate 128K --output \"#{DOWNLOAD_DIR}/#{platform_name}_#{category.gsub('+','_')}_00.html\"" 
		puts "curl \"http://www.gamespot.com/games.html?platform=#{platform_id}&category=#{category}&letter=[A-Z]\" --limit-rate 128K --output \"#{DOWNLOAD_DIR}/#{platform_name}_#{category.gsub('+','_')}_#1.html\"" 

   end
end