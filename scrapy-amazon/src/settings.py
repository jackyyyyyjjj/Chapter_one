# Scrapy settings for amazonUrlProductListSpider project


BOT_NAME = 'amazonUrlProductListSpider'

SPIDER_MODULES = ['src.spiders']
NEWSPIDER_MODULE = 'src.spiders'
RANDOM_UA_PER_PROXY = True
FAKEUSERAGENT_FALLBACK = 'Mozilla'

DOWNLOAD_DELAY = 5
# Obey robots.txt rules
ROBOTSTXT_OBEY = False
# Enable and configure the AutoThrottle extension (disabled by default)
# See https://docs.scrapy.org/en/latest/topics/autothrottle.html
AUTOTHROTTLE_ENABLED = True
# The initial download delay
AUTOTHROTTLE_START_DELAY = 5
# The maximum download delay to be set in case of high latencies
AUTOTHROTTLE_MAX_DELAY = 60



DOWNLOADER_MIDDLEWARES = {
    'scrapy.downloadermiddlewares.useragent.UserAgentMiddleware': None,
    'scrapy_user_agents.middlewares.RandomUserAgentMiddleware': 600,
    # 'amazonUrlProductListSpider.middlewares.AmazonUrlProductListSpiderDownloaderMiddleware': 543,
    'scrapy_proxy_pool.middlewares.ProxyPoolMiddleware': 410,
    'scrapy_proxy_pool.middlewares.BanDetectionMiddleware': 420,
}


ITEM_PIPELINES = {
    # 'amazonUrlProductListSpider.pipelines.PostgresPipeline': 300,
    # 'amazonUrlProductListSpider.pipelines.DuplicatesPipeline': 400,
    'src.pipelines.MongoPipeline': 300,

                  }


# COOKIES = {'session-id-time': '2082787201l', 'i18n-prefs': 'USD',
#            'session-id': '142-9723185-7096359', 'csm-hit': 'tb:s-TBNBSGGG2DTDACWFT2JG|1572104186431&t:1572104193942&adb:adblk_yes', 'sp-cdn': '"L5Z9:CN"', 'session-token': 'NtXSk4TNeLL1ywfKV+TvuhmxatgSa0yrUMVDxOzt0g6CAMeI6LkpgnQrcoU1asoE+pKF7ldrZnErq1dycNPGtszkRh03Wmo07Omhxs4OsROir2zQn4T5AtJAkn+RqVL8XB6izSJHsI0OWrp6to8bsr9AAw/4tLCFpEsnIh7nzYE0aDnZRQdyKCRbZbIxQTZg42jrFYHQH21c0ePPk9d0oC3feWEYOqh5KmCr5RWv8+xnCTX7kqpCELI9Qbsz1VKR', 'ubid-main': '135-5055030-7258235', 'x-wl-uid': '1iBt/JjYEoFYF+hGe2aCjWjyE0SGZ8B4QyX2KaTJl47LFamTRWYPbh4mcm/D2kLypor/oEsLBxqI', 'lc-main': 'zh_CN'}

#
MONGO_URI = 'localhost'

MONGO_DATABASE = 'taobao'

PORT =27017

# SQLITE_FILE = 'Amazon_product.sqlite'
# SQLITE_TABLE = 'amazon_product'

