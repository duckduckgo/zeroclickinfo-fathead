#! env python
# -*- coding: utf-8 -*-

from bs4 import BeautifulSoup
import logging
import os
import re

# TODO look for a better way to enforce it
import sys
reload(sys)
sys.setdefaultencoding('utf-8')

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

OUTPUT_FILE = 'output.txt'
INDEXES = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
WIKIPEDIA_URL = 'https://wikipedia.org'
WIKIPEDIA_LIST_URL = 'https://en.wikipedia.org/wiki/List_of_airports_by_IATA_code:_'

def append_period(text):
	""" Append a period at the end of the sentence"""
	if text[-1] == '\"':
		return text[0:-1]+'.\"'
	return text

class Airport(object):
	iata_abstract_format     = 'The "{0}" IATA airport code corresponds to {2} in {3}'
	icao_abstract_format     = 'The "{1}" ICAO airport code corresponds to {2} in {3} and the IATA code is "{0}"'
	name_abstract_format     = 'The IATA code for the {2} is "{0}"'
	location_abstract_format = 'The IATA code for the {2} near {3} is "{0}"'
	abstract_icao_format     = ' and the ICAO code is "{1}"'

	""" Contains informations about an Airport"""
	def __init__(self, name, iata, icao, location, index_letter):
		self.name = name
		self.iata = iata
		self.icao = icao
		self.location = location
		self._index_letter = index_letter
		self.international_airport_name = None
		self.name_with_airport = None 

		#logger.debug(self.name+';'+self.iata+';'+self.icao+';'+self.location+';'+self.index_letter)
		self.abstract_icao_part = ''
		if self.icao != '':
			self.abstract_icao_part = self._format(Airport.abstract_icao_format)

		# Put name with airport and international airport
		self.name_with_airport = self.name
		if self.name_with_airport.find('Airport') == -1:
			self.name_with_airport += ' Airport'

		index = self.name_with_airport.rfind(' International Airport')
		if index > 0:
			self.international_airport_name = self.name_with_airport[0:index]

		self.airport_location_name = None
		if self.location != None:
			location_names = self.location.split(',')
			if len(location_names) > 0:
				self.airport_location_name = location_names[0]+' Airport'
				if self.airport_location_name == self.name:
					self.airport_location_name = None

		# remove redundancy in airports/location names
		if self.name_with_airport.find('airports in ') != -1:
			self.name_with_airport = 'airports'

	def _format(self,string):
		return string.format(self.iata,self.icao,self.name_with_airport,self.location)

	def add_iata(self,output):
		abstract = self._format(Airport.iata_abstract_format)+self.abstract_icao_part
		if self.iata != None and len(self.iata) != 0:
			fields = self._getFields(self.iata,'A',append_period(abstract))
			output.append('%s' % ('\t'.join(fields)))

	def add_icao(self,output):
		abstract = self._format(Airport.icao_abstract_format)
		if self.icao != None and len(self.icao) != 0:
			fields = self._getFields(self.icao,'A',append_period(abstract))
			output.append('%s' % ('\t'.join(fields)))

	def add_name(self,output):
		abstract = self._format(Airport.name_abstract_format)+self.abstract_icao_part
		if self.name != None and len(self.name) != "":
			fields = self._getFields(self.name,'A',append_period(abstract))
			output.append('%s' % ('\t'.join(fields)))

	def add_location(self,output):
		abstract = self._format(Airport.location_abstract_format)+self.abstract_icao_part
		if self.airport_location_name != None:
			fields = self._getFields(self.airport_location_name,'A',append_period(abstract))
			output.append('%s' % ('\t'.join(fields)))

	def add_redirects(self,output):
		if self.international_airport_name != None:
			fields = self._getFields(self.international_airport_name,'R')
			fields[2] = self.name_with_airport
			fields[12] = ''
			output.append('%s' % ('\t'.join(fields)))

	def _getFields(self,name,linetype,abstract=''):
		return [name,	  # $unique_name
				linetype, # $type 
				'',	 # $redirect
				'',	 # $otheruses
				'',	 # $categories
				'',	 # $references
				'',	 # $see_also
				'',	 # $further_reading
				'',	 # $external_links
				'',	 # $disambiguation
				'',	 # images
				abstract,	 # abstract
				WIKIPEDIA_LIST_URL+self._index_letter] # source url


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
			if len(data) != 4: # partial table heading
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
					data[0].getText(),  # IATA
					data[1].getText(),  # ICAO
					data[3].getText(),
					self.index_letter)) # Name

if __name__ == '__main__':
	with open(OUTPUT_FILE, 'w') as output:
		airports = []

		# parse all
		for i in INDEXES:
			parser = Parser(i)
			logger.debug("Index: "+i)
			parser.get_airports()
			airports += parser.airports

		# print all
		for airport in airports:
			strings = []
			airport.add_iata(strings) # safe with no disambiguation needed
			airport.add_icao(strings) # safe with no disambiguation needed
			airport.add_name(strings) # safe with no disambiguation needed
			airport.add_location(strings)
			airport.add_redirects(strings) 
			output.write('\n'.join(strings)+'\n')

