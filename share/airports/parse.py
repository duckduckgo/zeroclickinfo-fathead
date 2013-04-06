# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import logging
import os
import re

# The "APH" airport code corresponds to the Jacksonville International
# Airport in Jacksonville, FL, United Sates.

# Limit the entries to the code (e.g. 'JAX' ) and the airport name +
# 'code' (e.g. 'Jacksonville International Airport code'). The latter
# being a redirect to the former. We could also include one with the
# word 'airport' removed (e.g. 'Jacksonville International code').
# Having the result for the city name would cover too many searches
# that aren't looking for the airport code.

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger()

WIKIPEDIA_URL = 'https://wikipedia.org'
WIKIPEDIA_LIST_URL = 'https://en.wikipedia.org/wiki/List_of_airports_by_IATA_code:_'

def replace_all(text, terms):
	""" Replaces all terms contained
	in a dict """
	for _from, _to in terms.items():
		text = text.replace(_from, _to)
	return text


class Airport(object):
	""" Contains informations about an Airport"""
	def __init__(self, name, iata, icao, location, index_letter):
		self.name = name
		self.iata = iata
		self.icao = icao
		self.location = location
		self.index_letter = index_letter 

	def __str__(self):
		output = ""
		logger.debug(self.name+';'+self.iata+';'+self.icao+';'+self.location+';'+self.index_letter)
		fields = [
				'',	 # $unique_name
				'A', # $type 
				'',	 # $redirect
				'',	 # $otheruses
				'',	 # $categories
				'',	 # $references
				'',	 # $see_also
				'',	 # $further_reading
				'',	 # $external_links
				'',	 # $disambiguation
				'',	 # images
				'',	 # abstract
				WIKIPEDIA_LIST_URL+self.index_letter # source url
				]

		if self.icao != '':
			abstract_icao_part = ' and the ICAO code is \"'+self.icao+'\"'
		else:
			abstract_icao_part = ''

		name_with_airport = self.name
		if name_with_airport.find('Airport') == -1:
			name_with_airport += ' Airport'

		# remove redundancy in airports/location names
		if name_with_airport.find('airports in ') != -1:
			name_with_airport = 'airports'


		iata_abstract = 'The IATA \"'+self.iata+'\" airport code corresponds to '+name_with_airport+' in '+self.location+abstract_icao_part+'.'
		icao_abstract = 'The ICAO \"'+self.icao+'\" airport code corresponds to '+name_with_airport+' in '+self.location+' and the IATA code is \"'+self.iata+'\".'
		name_abstract = 'The IATA code for the '+name_with_airport+' is \"'+self.iata+'\"'+abstract_icao_part+'.'
		location_abstract = 'The IATA code for the '+name_with_airport+' near '+self.location+' is \"'+self.iata+'\"'+abstract_icao_part+'.'

		fields[0] = self.iata
		fields[11] = iata_abstract
		if self.iata != None and len(fields[0]) != 0:
			output += '%s' % ('\t'.join(fields))

		fields[0] = self.icao
		fields[11] = icao_abstract
		if self.icao != None and len(self.icao) != 0:
			output += '\n%s' % ('\t'.join(fields))

		fields[0] = self.name
		fields[11] = name_abstract
		if self.name!= None and len(self.name) != "":
			output += '\n%s' % ('\t'.join(fields))+'\n'

		if self.location != None:
			location_names = self.location.split(',')
			if len(location_names) > 0:
				airport_location_name = location_names[0]+' Airport'
				if airport_location_name != self.name:
					fields[0] = airport_location_name
					fields[11] = location_abstract
					output += '%s' % ('\t'.join(fields))+'\n'

		return output


class Parser(object):
	""" Parses a HTML file to get all the airports codes """

	def __init__(self, index_letter):
		self.soup = BeautifulSoup(open('download/'+index_letter), from_encoding='utf-8')
		self.index_letter = index_letter

	def get_airports(self):
		self.airports = []
		table = self.soup.find_all('table')[1]
		line_number = 0

		for row in table.find_all('tr')[1::]:
			line_number+=1
			data = row.find_all('td')
			if (len(data) != 4): # partial table heading
				continue

			# check if data[3] has no link look in 
			airport_link = data[2].find('a')
			if airport_link == None:
				airport_link = data[3].find('a')
			if airport_link != None:
				airport_name = airport_link.getText()

			#logger.debug(data)
			self.airports.append(
				Airport(
					airport_name,
					data[0].getText(),		# IATA
					data[1].getText(),		# ICAO
					data[3].getText(),
					self.index_letter		# Name
					))

if __name__ == '__main__':
	indexes = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	with open('output.txt', 'w') as output:
		for i in indexes:
			parser = Parser(i)
			logger.debug("Index: "+i)
			parser.get_airports()
			for airport in parser.airports:
				output.write(airport.__str__().encode('utf-8'))

