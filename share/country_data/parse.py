import csv
LATEST_YEAR = 2015
OLDEST_YEAR = 2005

with open('download/gdp.csv', 'rb') as csvfile:
     gdpreader = csv.DictReader(csvfile)
     latest_gdps = {} 
     latest_gdp_year = {}
     for row in gdpreader:
         for year in xrange(OLDEST_YEAR, LATEST_YEAR):
             if row[str(year)] != "":
                 latest_gdps[row["Country Name"]] = row[str(year)]
             latest_gdp_year[row["Country Name"]] = str(year - 1)


class CountryDataItem:
  def __init__(self, country, data_type, data_value, year):
    self.country = country
    self.data_type = data_type
    self.data_value = str(data_value)
    self.year = year

  def __str__(self):

    fields = [ "%s of %s" % (self.data_type, self.country),  #title
               "A", #type
               "",
               "",
               "",  #categories
               "",
               "",  #see_also
               "",
               "",  #external_links
               "",
               "",  #images
               "The %s of %s in %s was %s" % (self.data_type, self.country, self.year, self.data_value),
               "http://data.worldbank.org/country/" + self.country.replace(" ","-")
             ]

    output = "%s\n" % ("\t".join(fields))

    return output

with open("output.txt", "wt") as output_file:
    for country, gdp in latest_gdps.items():        
        item = CountryDataItem(country, "GDP (in US $)", gdp, latest_gdp_year[country])
        output_file.write(str(item))
