#!/usr/bin/ruby

def single_city(tokens)
	[
    tokens[1], # title ; name of the city, in English. tokens[2] contains the name of the city, in the local language.
    'A', # type
    nil, # redirect
    nil, # otheruses
    nil, # categories
    nil, # references
    nil, # see also
    nil, # further reading
    nil, # external links
    nil, # disambiguation
    "[[Image:http://www.expatistan.com/images/expatistan_icon_97x97.png]]", # images
    "#{tokens[1]} has a cost of living index of #{tokens[3]} in Expatistan's scale (Prague, Czech Republic = 100)", # abstract
    tokens[4], # source url
  ].join("\t")
end

def comparison_abstract(city_1,city_2,difference)
	if difference > 0.96 && difference < 1.04
		readable_difference = "about the same as"
  else
		rounded_difference = ((1 - difference).abs * 100).round
		readable_difference = "#{rounded_difference}% #{difference > 1 ? 'more expensive' : 'cheaper'} than"
  end
	"Cost of living in #{city_1} is #{readable_difference} in #{city_2}."
end

def comparison(tokens)
	[
    "#{tokens[1]} ; #{tokens[3]}", # title ; names of the cities whose cost of living is compared. tokens[2] and tokens[4] contain the local name of the cities.
    'A', # type
    nil, # redirect
    nil, # otheruses
    nil, # categories
    nil, # references
    nil, # see also
    nil, # further reading
    nil, # external links
    nil, # disambiguation
    "[[Image:http://www.expatistan.com/images/expatistan_icon_97x97.png]]", # images
    comparison_abstract(tokens[1],tokens[3],tokens[5].to_f), # abstract
    tokens[6], # source url
  ].join("\t")
end

File.open("./data/zeroclickinfo.dump", "r") do |dump|
	while (line = dump.gets)
		tokens = line.split("\t")
		puts case tokens[0]
		  when "single_city" then single_city(tokens)
		  when "comparison" then comparison(tokens)
			else raise "Unkown entry - [#{tokens[0]}] : #{line}"
		end
  end
end
