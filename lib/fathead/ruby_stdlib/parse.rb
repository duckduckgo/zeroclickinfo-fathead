require 'rubygems'
require 'nokogiri'
require 'open-uri'

base_url = 'https://ruby-doc.org/stdlib-2.4.0/'
toc = Nokogiri::HTML(open('https://ruby-doc.org/stdlib-2.4.0/toc.html'))
items = toc.css('a.mature')

items.each do |item| # Looping through the Index URLs
    item_url = base_url + item['href']

    begin
        doc_index = Nokogiri::HTML(open(item_url))
    rescue OpenURI::HTTPError
    end

    next if doc_index.nil? || doc_index.css('#class-index .entries a').nil? || doc_index.css('#class-index .entries a').empty?

        class_entries = doc_index.css('#class-index .entries a')

        class_entries.each do |entry| # Looping through Classes
            class_url = item_url.chomp('index.html') + entry['href']
            class_doc = Nokogiri::HTML(open(class_url))
            class_related = item_url

            class_title = class_doc.css('h1').text.gsub(/::/, ' ')
            if class_doc.css('#description p').nil?
                class_description = 'No description provided'
            else
                class_description = class_doc.css('#description p').text.tr("\n", ' ').tr("\r", ' ')
            end
            open('output.txt', 'a') { |f| f.print class_title + "\t" + 'A' + "\t" + ' ' + "\t" + ' ' + "\t" + 'Ruby Class' + "\t" + ' ' + "\t" + class_related + "\t" + ' ' + "\t" + ' ' + "\t" + ' ' + "\t" + ' ' + class_description + class_url + "\n" }
        end

    next if doc_index.nil? || doc_index.css('#method-index .entries p').nil? || doc_index.css('#method-index .entries p:not(.nodoc)').empty?

    methods_entries = doc_index.css('#method-index .entries a').first

    method_url = item_url.chomp('index.html') + methods_entries['href']
    method_doc = Nokogiri::HTML(open(method_url))
    method_doc_detail = method_doc.css('.method-detail')

    method_id =
    method_related = item_url

    next if method_doc_detail.css('.method-detail p:not(.nodoc)').empty?

    method_doc_detail.each do |id|
        method_id = id['id']

        next if method_doc.at("##{method_id} span").text.gsub(/::/, ' ').include? "click to toggle source"

          method_title = method_doc.at("##{method_id} span").text.gsub(/::/, ' ')

        if method_doc.at('#' + "#{method_id} p").nil?
            method_description = 'No description provided'
        else
            method_description = method_doc.at('#' + "#{method_id} p").text.tr("\n", ' ').tr("\r", ' ')
        end
        open('output.txt', 'a') { |f| f.print method_title + "\t" + 'A' + "\t" + ' ' + "\t" + ' ' + "\t" + 'Ruby Method' + "\t" + ' ' + "\t" + method_related + "\t" + ' ' + "\t" + ' ' + "\t" + ' ' + "\t" + ' ' + method_description + method_url + "\n" }
    end
end
