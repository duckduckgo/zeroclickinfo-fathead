#!/usr/bin/env ruby

BASE_URL = "http://www.madehow.com/"

class MadeHowItem
  attr_accessor :name, :link, :desc
  
  def initialize(name, link, desc=nil)
    @name = name
    @link = link
    @desc = desc
  end
  
  def to_s
    [ @name,
      "A",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      @desc,
      @link ].join "\t"
  end
end

items = []
Dir.entries("download").each do |file|
  next if file =~ /^\./
  
  File.open("download/#{file}", 'r') do |f|
    item = nil
    f.each_line do |line|
      if item && (line =~ /<p>.+<\/p>/)
        # Use at most 3 sentences.
        contents = line.match(/<p>(.+)<\/p>/)[1]
        md = contents.scan(/(.+?\.)(\s|$)/)

        sentences = []
        md.each do |m|
          sentences << m[0]
        end

        item.desc = sentences[0..2].join(" ")
        items << item
        item = nil
        next
      end
      if (m = line.match(/<h2><a href="(\S+)">(.+)<\/a>/))
        item = MadeHowItem.new(m[2], BASE_URL + file + "/" + m[1])
        next
      end
    end
  end
end

File.open("output.txt", "w") do |f|
  items.each do |i|
    f.puts i
  end
end
