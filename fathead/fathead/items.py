# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

from scrapy.item import Item, Field

class FatheadItem(Item):
    title = Field()
    _type = Field()
    redirect = Field()
    other_uses = Field()
    categories = Field()
    references = Field()
    see_also = Field()
    further_reading = Field()
    external_links = Field()
    disambiguation = Field()
    images = Field()
    abstract = Field()
    source_url = Field()
    
