require 'rubygems'
require 'nokogiri'
require 'open-uri'

# STDLIB Docs

stdlib_url = 'https://ruby-doc.org/stdlib-2.4.0/'
stdlib_toc = Nokogiri::HTML(open(stdlib_url + '/toc.html'))
entries = stdlib_toc.css('a.mature')

entries.each do |entry| # Looping through the Index URLs
  entry_url = stdlib_url + entry['href']
  begin
    doc_index = Nokogiri::HTML(open(entry_url))
  rescue OpenURI::HTTPError
  end
  open('stdlib.txt', 'a') { |f| f.print entry.text + "\n" }

  unless doc_index.nil?

    class_entries = doc_index.css('#class-index .entries a')

    class_entries.each do |entry|
      open('stdlib.txt', 'a') { |f| f.print entry.text + "\n" }
    end

  end

  next if doc_index.nil?

  methods_entries = doc_index.css('#method-index .entries a')

  methods_entries.each do |entry|
    open('stdlib.txt', 'a') { |f| f.print entry.text + "\n" }
  end
end

# Core Docs

core_url = 'http://ruby-doc.org/core-2.4.0/'
core_toc = Nokogiri::HTML(open(core_url))
core_classes = core_toc.css('#class-index .entries .class a')
core_methods = core_toc.css('#method-index .entries a')

core_classes.each do |entry|
  open('core.txt', 'a') { |f| f.puts entry.text + "\n" }
end

core_methods.each do |entry|
  open('core.txt', 'a') { |f| f.puts entry.text + "\n" }
end
