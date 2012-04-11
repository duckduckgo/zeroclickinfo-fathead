#!/usr/bin/python
# -*- coding: utf-8 -*-

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

import lxml.html

#url = "http://en.wikipedia.org/wiki/List_of_circulating_currencies"
url = "https://secure.wikimedia.org/wikipedia/en/wiki/List_of_circulating_currencies"
output = "output.txt"

f = open(output, "w")

countries = {};     # country:[[currency, code] [currency, code],...]
country = ""        # store current country for each row
currency = ""       # store current currency for each row
iso_code = ""       # store current iso code for currency
description = ""    # store currency and iso code when saving into file

def add_currency(country, currency, iso_code, countries):
    "Add country into countries list"
    country = country.encode("utf8")
    if country in countries:
        countries[country].append([currency, iso_code])
    else:
        countries[country] = [[currency, iso_code]]

def clear_text(text):
    "Clear text of anotations in []. Like 'Ascension pound[A]' contains [A]"
    start = text.find("[")
    if start != -1:
        text = text[:start]
    return text

tree = lxml.html.parse("download/raw.dat").getroot()
tables = tree.find_class("wikitable sortable")
for table in tables:
    for row in table.findall('tr'):
        cells = row.findall('td')
        if len(cells) == 6:
            country = cells[0].text_content()
            currency = cells[1].text_content()
            iso_code = cells[3].text_content()

        if len(cells) == 5:
            currency = cells[0].text_content()
            iso_code = cells[2].text_content()
        
        currency = clear_text(currency)
        iso_code = iso_code if iso_code != "None" else ""
        
        if currency != "None" and currency != "":
            add_currency(country, currency, iso_code, countries)


for country in sorted(countries):
    for record in countries[country]:
        iso_code = "" if record[1] == "" else ("(" + record[1] + ")")
        currency = record[0]
        description = (currency + " " + iso_code).encode("utf8")
        f.write("\t".join([country,    # title
                    "",                # namespace
                    "", #url,          # url
                    description,       # description
                    "",                # synopsis
                    "",                # details
                    "",                # type
                    ""                 # lang
                   ])
           );
        f.write("\n");
f.close()
