# Define here the models for your scraped items
import scrapy
from scrapy.loader.processors import MapCompose, TakeFirst
from w3lib.html import remove_tags


def remove_whitespace(value):
    return value.strip().replace('\n', ' ')


def datetime(value):
    return value.replace('/', '-')


def slugify(value):
    return value.replace(' ', '-').replace(',', '') \
        .replace('(', '').replace(')', '').replace(':', '') \
        .replace('/', '-').replace('®', '')\
        .replace('"', '').replace('|', '')\
        .replace('ü', 'ue').replace('ä', 'ae')\
        .replace('%', '').replace('---', '-')\
        .replace(';', '').replace(' ', '')\
        .replace('™', '').replace('.', '')\
        .replace('+', '').replace('--', '-')\
        .replace('\'', '-').lower()


def clean_price(value):
    return value.replace('\xa0', ' ').replace('€', '').replace(',', '.')


def truncate(value):
    return value[0:69]


def clean_description(value):
    return value.replace('This fits your .', '').replace('Make sure this fits by entering your model number.', '')\
        .replace('P.when("ReplacementPartsBulletLoader").execute(function(module){ module.initializeDPX(); })', '')\
        .replace('Dieser Artikel passt für Ihre .', '')\
        .replace('Geben Sie Ihr Modell ein, um sicherzustellen, dass dieser Artikel passt. ', '')


def clean_title(value):
    return value.replace(' ', '')


def clean_popularity(value):
    return value.replace('Sternebewertungen', '').replace('.', '').replace('ratings', '')\
        .replace(',', '').replace('', '')


def clean_brand(value):
    return value.replace('Besuchen Sie den', '').replace('Marke:', '').replace('Visit the', '')\
        .replace('-Store', '').replace('Store', '').replace('Brand:', '')


def replace_shop_at_amazon_w_blank(value):
    return value.replace('Jetzt bei Amazon.de bestellen!', '')


def replace_amazon_w_meisteraffiliate(value):
    return value.replace('Amazon.de ', 'Meisteraffiliateshop.com')\
        .replace('Amazon.de:', 'Meisteraffiliateshop')


class AmazonUrlProductListSpiderItem(scrapy.Item):
    # define the fields for your item here like:
    collection = 'top_firms'

    big_small_html = scrapy.Field()
    # asin = scrapy.Field()
    item_detail = scrapy.Field()
    ASIN = scrapy.Field()
    detail_page_url = scrapy.Field()
    # availability = scrapy.Field()
    productOverview_feature= scrapy.Field()
    productDescription = scrapy.Field()
    # star_rating = scrapy.Field()
    # comments= scrapy.Field(input_processor=MapCompose(remove_whitespace),
        # output_processor=TakeFirst())