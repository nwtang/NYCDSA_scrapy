# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class MetacriticItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    game = scrapy.Field()
    console = scrapy.Field()
    release_date = scrapy.Field()
    developer = scrapy.Field()
    genre = scrapy.Field()
    reviewer = scrapy.Field()
    score = scrapy.Field()
