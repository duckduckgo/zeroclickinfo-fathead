#!/usr/bin/ruby

require 'rubygems'
require 'hpricot'

# load the requested file for scraping
doc = Hpricot(open(ARGV[0]))

# get all the table rows of the second table in the document
rows = doc.search("//table:nth(2)//tr")

# ignore the header row and the last two rows of fine print
rows.delete_at(0)
rows.delete_at(-1)
rows.delete_at(-1)

# convert each row to fathead format
rows.each do |row|
	
	# get the type, number, name, and link for this part
	paragraphs = row.search("p")
	part_type = paragraphs[0].inner_html
	part_number = paragraphs[1].at("a").inner_html	
	name_link = paragraphs[2].at("a")
	part_name = name_link.inner_html
	part_link = name_link['href']
	
	# first word of part name is conventionally part category
	# (see http://www.ldraw.org/Article340.html for formal category spec)
	# subparts names are typically prefixed by a tilde, which we omit
	part_category = part_name.split.first.delete("~")
	
	# strip the red/yellow/green icon from status
	paragraphs[3].search("img").remove
	part_status = paragraphs[3].inner_html.strip
	
	# part_path is used to generate links to preview images and .dat files
	part_path = case part_type
		when "Part" then "parts"
		when "Subpart" then "parts/s"
		when "Primitive" then "p"
		when "48-Segment Primitive" then "p/48"
		else ""
	end
	
	# - "NAME (NUMBER.dat - STATUS)"
	# - show part_number as LDraw path (eg "parts/s/NUMBER.dat")?
	# - other information such as author, revision history, CATEGORY/KEYWORD
	#   labels, and links to related/required files could be derived from the
	#   detail page for each part, which would require more page scraping,
	#   or also from actual files (best to report Tracker's interpretation)
	abstract = "#{part_name} (<a href=\"http://www.ldraw.org/library/unofficial/#{part_path}/#{part_number}.dat\">#{part_path}/#{part_number}.dat</a> - #{part_status})"
	
	# - part_number as title to respond to search queries for number
	#   (or use part_name to look up number based on query by name?)
	# - unclear what types are available/appropriate for line[1]
	# - image and source links output relative to ldraw.org
	puts "#{part_number}\tA\t\t\t#{part_category}\t\t\t\t\t\t/library/unofficial/images/#{part_path}/#{part_number}.png\t#{abstract}\t#{part_link}\n"

end
