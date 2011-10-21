#!/usr/bin/ruby
# ~ 2 minutes to complete

require 'rubygems'
require 'hpricot'
require 'open-uri'

DOWNLOAD_DIR='download'
puts "mkdir -p #{DOWNLOAD_DIR}"

# Find the categories
categories=[]
doc=Hpricot(open('http://plone.org/products'))
(doc/"//select[@name='getCategories']/option").each{|x|categories<< [x['value'], x.inner_text] unless x.inner_text=='All categories'}

#
# Find the number of records for each category.  This is required to set a limit for the start_int 
# (records returned in groups of 10 designated by the start index)
#
categories.each do |category|
	 doc2=Hpricot(open("http://plone.org/products?getCompatibility=any&path=/plone.org/products&portal_type=PSCProject&getCategories=#{category[0].gsub(' ','+')}&b_start:int=0"))
	 category << (doc2/"//p[@id='numreleases']").inner_text.strip.split(' ')[0]
	 sleep 3  # Being polite
end

# Write out the curl calls
categories.each do |c|
   # puts "# #{c[2]}\t\t#{c[1]} (#{c[0]})"
   puts "curl \"http://plone.org/products?getCompatibility=Plone%203&path=/plone.org/products&portal_type=PSCProject&getCompatibility=any&getCategories=#{c[0]}&b_start:int=[0-#{c[2]}:10]\" --limit-rate 128K --output \"#{DOWNLOAD_DIR}/#{c[1].gsub(' ','_').gsub('/','_')}_#1.html\""
end