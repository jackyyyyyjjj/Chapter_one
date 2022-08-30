from selenium import webdriver
from selenium.webdriver.common.by import By
import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from time import sleep
from selenium.webdriver.support import select
from selenium.webdriver.support.select import Select
from selenium.webdriver.support.ui import Select
import pandas as pd
from lxml import etree
import numpy as np
import sqlite3
import datetime

list = pd.read_excel('sales.xlsx')
# list = list.iloc[349:,]
# list = pd.read_excel('listsale.xlsx')
list['sales'] = list['sales'].astype(int)
# newlist = list[:,1].astype(int)
driver = webdriver.Chrome('D:/uea_onedrive/OneDrive - University of East Anglia/Documents/UEA_PHD/Chapter1/code/Robot_vcuum_data/patent_doc2vec/nlp_doc2vec_unsupervised_clustering-main/chromedriver.exe')
driver.get("https://amzscout.net/sales-estimator/")
# find_element(by=By.CSS_SELECTOR, value='#kw')
time.sleep(4)
button1 = driver.find_element(by=By.CSS_SELECTOR, value='#salesEstimator-behavior > div > div:nth-child(2) > div > button')
button1.click()
time.sleep(4)
button2 = driver.find_element(by=By.CSS_SELECTOR, value='#salesEstimator-category > li:nth-child(17)')
time.sleep(4)
button2.click()


# 建立空列表
# SHIF_list = []

def init_db1(dbpath):
    # initsql = "drop table if exists estsales"  #判断studentTable表是否存在，如果存在，则删除

    createsql = '''
        create table if not exists estsales
        (
            asin varchar,
            market_ids varchar ,
            sales varchar
        )
    '''                             #新建studentTable数据表

    conn = sqlite3.connect(dbpath)   #打开或创建 连接数据库文件
    cursor = conn.cursor()           #获取游标
    # cursor.execute(initsql)          # 执行SQL语句
    cursor.execute(createsql)        #执行SQL语句
    conn.commit()                    #提交数据库操作
    conn.close()                     #关闭数据库连接

#保存数据db
dbpath = "amazon_sales.sqlite"
init_db1(dbpath)                  #初始化数据库
conn = sqlite3.connect(dbpath)   #连接数据库文件
cur = conn.cursor()              #获取游标




for i in range(len(list.iloc[:,1])):  # 一共有11行数据
    sales_list = []
    marketids_list = []
    asin_list = []
    # 获取网页源码
    input = driver.find_element(by=By.CSS_SELECTOR, value='#salesEstimator-behavior > div > div:nth-child(3) > input')
    input.send_keys(list.iloc[i,0].astype(str))
    time.sleep(1)

    try:
        button3 = driver.find_element(by=By.CSS_SELECTOR,
                                      value='#salesEstimator-behavior > section > div > div > button')
        time.sleep(1)
        button3.click()
    except Exception as e:
        print('异常信息为：',e)
        time.sleep(62)
        button3 = driver.find_element(by=By.CSS_SELECTOR,
                                      value='#salesEstimator-behavior > section > div > div > button')
        # time.sleep(6)
        button3.click()
    time.sleep(3) #记住时刻控制时间的把我
    resp_text = driver.page_source
    # 数据解析
    page_html = etree.HTML(resp_text)

    # 开始把表格信息写入并做到能够循环
    sales_list.insert(1, page_html.xpath('//*[@id="salesEstimator-behavior_data"]/p/text()'))
    # date = datetime.datetime.strftime(list.iloc[i,1],"%Y-%m")
    marketids_list.insert(1,list.iloc[i,1])
    asin_list.insert(1, list.iloc[i, 2])
    a = {
        'asin' :asin_list,
        'market_ids' :marketids_list,
        'SHIFTDATE': sales_list[0]
    }
    try:
        offline_a = pd.DataFrame(a)
        data = offline_a.iloc[0, :]
        print(data)
        cur.execute("insert into estsales(asin,market_ids, sales)values(?, ?, ?)",
                    (data[0], data[1], data[2]))  # 执行SQL语句
        conn.commit()  # 提交数据库操作
        input.send_keys(Keys.CONTROL + 'a')
        time.sleep(1)
        input.send_keys(Keys.DELETE)  # DELETE INPUTS
    except Exception as e:
        print('异常信息为：', e)
        # 获取网页源码
        input.send_keys(Keys.CONTROL + 'a')
        # time.sleep(2)
        input.send_keys(Keys.DELETE)  # DELETE INPUTS
        # time.sleep(4)




# 点击下一页

# offline_a = pd.DataFrame(a)  # 忘了什么意思，但有dataframe应该是转成Excel那样的表格样式吧
# offline_a.to_excel('随便叫什么.xlsx', index=False)  # 保存到Excel

driver.quit()  #关闭webdriver
cur.close()
conn.close()       #关闭数据库连接





