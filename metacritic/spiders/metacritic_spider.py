from scrapy import Spider, Request
from metacritic.items import MetacriticItem
import re

class MetacriticSpider(Spider):
    name = 'metacritic_spider'
    allowed_domains = ['www.metacritic.com']
    consoles = ['ps','n64','dreamcast','ps2','gamecube','xbox','ps3','wii','xbox360','wii-u','ps4','switch','xboxone','pc']
    start_urls = ['https://www.metacritic.com/browse/games/release-date/available/{}/metascore'.format(console) for console in consoles]

    def start_requests(self):
        for console in self.consoles:
            url = 'https://www.metacritic.com/browse/games/release-date/available/{}/metascore'.format(console)
            yield Request(url=url, meta={'console':console}, callback=self.parse, dont_filter=True)
    
    def parse(self, response):
        console = response.meta['console']
        try:
            n_pages = int(response.xpath('//li[@class="page last_page"]/a/text()').extract_first())
            result_urls = ['https://www.metacritic.com/browse/games/release-date/available/{}/metascore'.format(console)]
            result_urls.extend(['https://www.metacritic.com/browse/games/release-date/available/{}/metascore?page={}'.format(console,str(x)) for x in range(1,n_pages)])
        except:
            result_urls = ['https://www.metacritic.com/browse/games/release-date/available/{}/metascore'.format(console)]
        for url in result_urls:
            yield Request(url=url, meta={'console':console}, callback=self.parse_result_page)

    def parse_result_page(self, response):
        console = response.meta['console']
        detail_urls = response.xpath('//div[@class="basic_stat product_title"]/a/@href').extract()
        for url in ['https://www.metacritic.com{}'.format(x) for x in detail_urls]:
            yield Request(url=url, meta={'console':console}, callback=self.parse_detail_page)

    def parse_detail_page(self, response):
        console = response.meta['console']
        game = response.xpath('//div[@class="product_title"]/a/h1/text()').extract()
        release_date = response.xpath('//li[@class="summary_detail release_data"]/span[@class="data"]/text()').extract()
        developer = response.xpath('//li[@class="summary_detail developer"]/span[@class="data"]/text()').extract_first().replace("\n","").replace(" ","")
        genre = ", ".join(response.xpath('//li[@class="summary_detail product_genre"]/span[@class="data"]/text()').extract())
        review_url = 'https://www.metacritic.com'+response.xpath('//a[@class="metascore_anchor"]/@href').extract_first()
        yield Request(url=review_url, meta={'console':console,'game':game,'release_date':release_date,'developer':developer,'genre':genre}, callback=self.parse_review_page)

    def parse_review_page(self, response):
        console = response.meta['console']
        game = response.meta['game']
        release_date = response.meta['release_date']
        developer = response.meta['developer']
        genre = response.meta['genre']
        reviews = response.xpath('//li[contains(@class,"review critic_review")]')
        for review in reviews:
            reviewer = review.xpath('.//div/div/div/div/div/div/div[@class="review_stats"]/div[@class="review_critic"]/div[@class="source"]/text()').extract()
            if reviewer == []:
                reviewer = review.xpath('.//div/div/div/div/div/div/div[@class="review_stats"]/div[@class="review_critic"]/div[@class="source"]/a/text()').extract()
            score = review.xpath('.//div/div/div/div/div/div/div[@class="review_stats"]/div[@class="review_grade"]/div/text()').extract()

            item = MetacriticItem()
            item['game'] = game
            item['console'] = console
            item['release_date'] = release_date
            item['developer'] = developer
            item['genre'] = genre
            item['reviewer'] = reviewer
            item['score'] = score

            yield item

        


        
