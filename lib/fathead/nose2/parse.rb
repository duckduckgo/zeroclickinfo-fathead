#!/usr/bin/env ruby
require 'nokogiri'
require 'open-uri'
require 'pry'

NOSE_HOME = "http://nose2.readthedocs.io/en/latest/getting_started.html"
topics = ARGV

topics.each do |topic|
  url = "http://nose2.readthedocs.io/en/latest/getting_started.html##{topic}"
  doc = Nokogiri::HTML(open(url))
  abstract = doc.css("##{topic}").css("p").to_s
  output_row = [topic, 'A', '', '', '', '', '', '', NOSE_HOME, '', '', abstract, url]

  File.open('output.txt', 'a') { |f| f.write(output_row) }
end
