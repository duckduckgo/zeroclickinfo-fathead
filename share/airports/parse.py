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
				'',		# $unique_name
				'A',	# $type 
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

		iata_abstract = 'The \"'+self.iata+'\" airport code corresponds to the '+self.location+' in '+self.name
		icao_abstract = 'The \"'+self.icao+'\" airport code corresponds to the '+self.location+' in '+self.name
		location_abstract = 'The \"'+self.location+'\" airport corresponds to the IATA '+self.iata+' and ICAO '+self.icao

		fields[0] = self.iata
		fields[11] = iata_abstract
		if self.iata != None and len(fields[0]) != 0:
			output += '%s' % ('\t'.join(fields)) + '\n'

		fields[0] = self.icao
		fields[11] = icao_abstract
		if self.icao != None and len(self.icao) != 0:
			output += '%s' % ('\t'.join(fields)) + '\n'

		fields[0] = self.location+' Airport'
		fields[11] = location_abstract
		if self.location != None and len(self.location) != "":
			output += '%s' % ('\t'.join(fields))
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
			airport_link = data[3].find('a')
			if airport_link == None:
				airport_link = data[2].find('a')
			else:
				airport_location = airport_link.getText()

			if airport_link != None:
				airport_link = airport_link['href']
				wikipedia_link = WIKIPEDIA_URL+airport_link
			else:
				wikipedia_link = ""

			#logger.debug(data)
			self.airports.append(
				Airport(
					data[3].getText(),		# Name
					data[0].getText(),		# IATA
					data[1].getText(),		# ICAO
					airport_location,
					self.index_letter
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

