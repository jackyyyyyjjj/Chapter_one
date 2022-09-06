from sqlalchemy import create_engine, Column, Numeric
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import (
    Integer, String, DateTime, Text)
from sqlalchemy.engine.url import URL

from src import settings

Base = declarative_base()


def db_connect():
    """
    Performs database connection using database settings from settings.py.
    Returns sqlalchemy engine instance
    """
    return create_engine(URL(**settings.POSTGRES_DATABASE))


def create_table(engine):
    Base.metadata.create_all(engine)


class ProductDB(Base):
    __tablename__ = "scraped_products"
    detail_page_url = Column('detail_page_url', Text())
    asin = Column('asin', String(10))
    uuid = Column('uuid', String(length=36), primary_key=True)
    price = Column('price', Numeric(10, 2))
    publisher = Column('publisher', String(50))
    manufacturer = Column('manufacturer', String(50))
    brand = Column('brand', String(50))
    medium_image = Column('medium_image', Text())
    large_image = Column('large_image', Text())
    popularity = Column('popularity', Numeric())
    featured = Column('featured', Integer(), default=1)
    category_id = Column('category_id', Integer())
    description = Column('description', Text())
    title = Column('title', Text())
    created_on = Column('created_on', DateTime())
    updated_on = Column('updated_on', DateTime())
    slug = Column('slug', Text())
    unavailable_after = Column('unavailable_after', DateTime())
    h1_heading = Column('h1_heading', String(60))
    meta_description = Column('meta_description', Text())
    meta_robots = Column('meta_robots', Integer(), default=1)
    large_image_alt = Column('large_image_alt', Text())
    medium_image_alt = Column('medium_image_alt', Text())
    description_de = Column('description_de', Text())
    description_en = Column('description_en', Text())
    detail_page_url_de = Column('detail_page_url_de', Text())
    detail_page_url_en = Column('detail_page_url_en', Text())
    slug_de = Column('slug_de', Text())
    slug_en = Column('slug_en', Text())
    title_de = Column('title_de', String(60))
    title_en = Column('title_en', String(60))
    h1_heading_de = Column('h1_heading_de', String(60))
    h1_heading_en = Column('h1_heading_en', String(60))
    meta_description_de = Column('meta_description_de', Text())
    meta_description_en = Column('meta_description_en', Text())







