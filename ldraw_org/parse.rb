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

	# strip the red/yellow/green icon from status
	paragraphs[3].search("img").remove
	part_status = paragraphs[3].inner_html.strip
	
	# generate links to preview images and actual .dat files
	case part_type
		when "Part"
			image_link = "/library/unofficial/images/parts/#{part_number}.png"
			file_link = "http://www.ldraw.org/library/unofficial/parts/#{part_number}.dat"
		when "Subpart"
			image_link = "/library/unofficial/images/parts/s/#{part_number}.png"
			file_link = "http://www.ldraw.org/library/unofficial/parts/s/#{part_number}.dat"
		when "Primitive"
			image_link = "/library/unofficial/images/p/#{part_number}.png"
			file_link = "http://www.ldraw.org/library/unofficial/p/#{part_number}.dat"
		when "48-Segment Primitive"
			image_link = "/library/unofficial/images/p/48/#{part_number}.png"
			file_link = "http://www.ldraw.org/library/unofficial/p/48/#{part_number}.dat"
		else ""
	end
	
	# - "NAME (NUMBER.dat - STATUS)"
	# - show part_number as LDraw path (eg "parts/s/NUMBER.dat")?
	# - other information such as author, revision history, CATEGORY/KEYWORD
	#   labels, and links to related/required files could be derived from the
	#   detail page for each part, which would require more page scraping,
	#   or also from actual files (best to report Tracker's interpretation)
	abstract = "#{part_name} (<a href=\"#{file_link}\">#{part_number}.dat</a> - #{part_status})"
	
	# - part_number as title to respond to search queries for number
	#   (or use part_name to look up number based on query by name?)
	# - unclear what types are available/appropriate for line[1]
	# - image and source links output relative to ldraw.org
	puts	"#{part_number}\t" + \
			"A\t" + \
			"\t" + \
			"\t" + \
			"Unofficial LDraw Part\t" + \
			"\t" + \
			"\t" + \
			"\t" + \
			"\t" + \
			"\t" + \
			"#{image_link}\t" + \
			"#{abstract}\t" + \
			"#{part_link}\n"
end
