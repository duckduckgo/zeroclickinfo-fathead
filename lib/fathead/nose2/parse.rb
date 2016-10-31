#!/usr/bin/env ruby
require 'nokogiri'
require 'open-uri'

NOSE_HOME = "http://nose2.readthedocs.io/en/latest/getting_started.html"
topics = ARGV

topics.each do |topic|
  url = "http://nose2.readthedocs.io/en/latest/getting_started.html##{topic}"
  doc = Nokogiri::HTML(open(url))
  abstract = doc.css("##{topic}").css("p").to_s
  abstract = abstract.gsub("\n", " ").gsub(/class\=\S{1,2}[a-z]*(\s[a-z]*)?\S{1,2}/, "")
  output_row = [topic, 'A', '', '', '', '', '', '', NOSE_HOME, '', '', abstract, url].join("\t")
  File.open('output.txt', 'a') { |f| f.write(output_row + "\n") }
end
