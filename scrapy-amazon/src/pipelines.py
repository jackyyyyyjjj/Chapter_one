# ---------------POSTGRES-------------------------------#

# from sqlalchemy import create_engine
# from sqlalchemy.engine import URL
# from amazonUrlProductListSpider import settings
# from sqlalchemy.orm import sessionmaker
# from amazonUrlProductListSpider.models import ProductDB, create_table, db_connect


# class PostgresPipeline(object):
#     def __init__(self):
#         """
#         Initializes database connection and sessionmaker.
#         Creates product table.
#         """
#         engine = db_connect()
#         create_table(engine)
#         self.Session = sessionmaker(bind=engine)
#
#     def process_item(self, item, spider):
#         """Save products in the database.
#
#         This method is called for every item pipeline component.
#         """
#         session = self.Session()
#         self.Session.configure(bind=create_engine(URL(**settings.POSTGRES_DATABASE)))
#         product_db = ProductDB(**item)
#
#         try:
#             if session.query(ProductDB).filter_by(asin=item['asin']).first() is None:
#                 session.add(product_db)
#                 session.commit()
#
#         except:
#             session.rollback()
#             raise print('ERROR!!! Some Items May Not Have Been Added!')
#         finally:
#             session.close()
#         return item

# --------------END--POSTGRES--------------------------------#


# --------------CSV------------------------------------------#

# from scrapy.exporters import CsvItemExporter
# from scrapy import signals
#
#
# class CSVPipeline(object):
#     @classmethod
#     def from_crawler(cls, crawler):
#         pipeline = cls()
#         crawler.signals.connect(pipeline.spider_opened, signals.spider_opened)
#         crawler.signals.connect(pipeline.spider_closed, signals.spider_closed)
#         return pipeline
#
#     def spider_opened(self, spider):
#         self.file = open('output.csv', 'w+b')
#         self.exporter = CsvItemExporter(self.file)
#         self.exporter.start_exporting()
#
#     def spider_closed(self, spider):
#         self.exporter.finish_exporting()
#         self.file.close()
#
#     def process_item(self, item, spider):
#         self.exporter.export_item(item)
#         return item

# --------------END--CSV--------------------------------#

# --------------pymongo------------------------------------------#
import pymongo

class MongoPipeline(object):
    def __init__(self, mongo_uri, mongo_db):
        self.mongo_uri = mongo_uri
        self.mongo_db = mongo_db

    @classmethod
    def from_crawler(cls, crawler):
        return cls(
            mongo_uri=crawler.settings.get('MONGO_URI'),
            mongo_db=crawler.settings.get('MONGO_DATABASE')
        )

    def open_spider(self, spider):
        self.client = pymongo.MongoClient(self.mongo_uri)
        self.db = self.client[self.mongo_db]
        self.db.authenticate("Junjun", "123456")
        # self.db[UserItem.collection].create_index([('id', pymongo.ASCENDING)])
        # self.db[WeiboItem.collection].create_index([('id', pymongo.ASCENDING)])

    def close_spider(self, spider):
        self.client.close()

    def process_item(self, item, spider):
        self.db[item.collection].insert(dict(item))



# --------------END--pymongo--------------------------------#

# --------------Begin--sqlite--------------------------------#

# import sqlite3
#
#
# class Sqlite3Pipeline(object):
#
#     def __init__(self, sqlite_file, sqlite_table):
#         self.sqlite_file = sqlite_file
#         self.sqlite_table = sqlite_table
#
#     @classmethod
#     def from_crawler(cls, crawler):
#         return cls(
#             sqlite_file=crawler.settings.get('SQLITE_FILE'),  # 从 settings.py 提取
#             sqlite_table=crawler.settings.get('SQLITE_TABLE', 'items')
#         )
#
#     def open_spider(self, spider):
#         self.conn = sqlite3.connect(self.sqlite_file)
#         self.cur = self.conn.cursor()
#
#     def close_spider(self, spider):
#         self.conn.close()
#
#     def process_item(self, item, spider):
#         self.cur = self.conn.cursor()
#         insert_sql = "insert into {0}({1}) values ({2})".format(self.sqlite_table,
#                                                                 ', '.join(item.keys()),
#                                                                 ', '.join(['?'] * len(item.keys())))
#         mytuple = tuple(item.values())
#         try:
#             self.cur.execute(insert_sql, mytuple)
#         except:
#             pass
#         self.conn.commit()
#         # self.cur.close()
#         return item