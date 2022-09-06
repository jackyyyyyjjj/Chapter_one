from datetime import datetime
import scrapy
from src.getPymongo_data import *
# from scrapy.crawler import CrawlerProcess
from scrapy.loader import ItemLoader
from src.items import AmazonUrlProductListSpiderItem
import uuid as uuid_lib
import pandas as pd
import re

#from src.middlewares import driver
# from src.settings import MONGO_URI,MONGO_DATABASE,PORT
BASE_URL = 'https://www.amazon.com'
Review_BaseUrl = "https://www.amazon.com/dp/{}"
class AmazonUrlProductListSpider(scrapy.Spider):
    name = 'amazon_url_product_list_spider'
    allowed_domains = ['amazon.com']

    def start_requests(self):
        # Review_BaseUrl = "https://www.amazon.com/product-reviews/{}/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&pageNumber={}&reviewerType=all_reviews"
        asins_list = pd.read_excel('D:/uea_onedrive/OneDrive - University of East Anglia/Documents/UEA_PHD/First_charpter/Top_firms/scrapy-amazon-the_description_product_2/src/spiders/asin.xlsx')
        asins = asins_list.iloc[:, 0]

        for asin in asins:
            url = Review_BaseUrl.format(asin)
            start_urls = url
            yield scrapy.Request(start_urls,callback=self.parse,meta ={'asin':asin})


    def parse(self, response):

        amazon_item = AmazonUrlProductListSpiderItem()
        """
        商品详情
        :param asin:
        :return:
        """

        try:
            # 获取ASIN
            pattern = re.compile(r'/dp/(.*)')
            ASIN=pattern.findall(response.url)
            amazon_item["ASIN"] = ASIN
        except Exception as e:
            pass
        try:
            # 获取detail_page_url
            detail_page_url = response.url
            amazon_item["detail_page_url"] = detail_page_url
        except Exception as e:
            pass
        # try:
        #     # 获取availability
        #     availability = response.xpath("//div[@id='availability']/span/text()").extract()[0].strip().replace('\n','')
        #     amazon_item["availability"] = availability
        # except Exception as e:
        #     pass

        try:
            # get all the product overview feature
            productOverview_feature= response.xpath('string(//div[@id="productOverview_feature_div"])').extract()
            amazon_item["productOverview_feature"] = str(productOverview_feature)
        except Exception as e:
            pass


        try:
            # 有表格
            big_small_html= response.xpath('string(//*[@id="productDetails_detailBullets_sections1"])').extract()
            amazon_item["big_small_html"] = big_small_html
        except Exception as e:
            pass

        try:
            # 有表格
            productDescription= response.xpath('string(//*[@id="productDescription"])').extract()
            amazon_item["productDescription"] = productDescription
        except Exception as e:
            pass
        try:
            # 有表格
            item_detail= response.xpath('string(//*[@id="feature-bullets"])').extract()
            amazon_item["item_detail"] = item_detail
        except Exception as e:
            pass
        print("爬取成功")
        return amazon_item




