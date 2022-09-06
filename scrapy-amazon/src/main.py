"""
@author: Junjun_Zhang
@contact: zhangjunjunessexec@gmail.com
@file: mian.py
@time: 2021/10/31 8:42
"""

from scrapy.cmdline import execute

execute(["scrapy", "crawl", "amazon_url_product_list_spider"])