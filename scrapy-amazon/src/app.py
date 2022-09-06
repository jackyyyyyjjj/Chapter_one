
"""
@author: Junjun_Zhang
@contact: zhangjunjunessexec@gmail.com
@file: app.py
@time: 2022/4/5 23:43
"""
import requests
import json
import sqlite3 as sqlite
conn = sqlite.connect('patent_db.sqlite')
# Request Parameters
store = "android"       # Could be either "android" or "itunes".
country_code = "US"     # Two letter country code.
date = "2022-04-04"     # Date to check app details against (YYYY-MM-DD).

req_params = {"date": date,
              "country": "US"}

# Auth Parameters
username = "9cbec3c8f6faad213bc52074f90c902951673855"  # Replace {API_KEY} with your own API key.
password = "X"          # Password can be anything.

# Request URL
request_url = "https://api.appmonsta.com/v1/stores/%s/details.json" % store

# This header turns on compression to reduce the bandwidth usage and transfer time.
headers = {'Accept-Encoding': 'deflate, gzip'}

# Python Main Code Sample
response = requests.get(request_url,
                        auth=(username, password),
                        params=req_params,
                        headers=headers,
                        stream=True)

print(response.status_code)
for line in response.iter_lines():
  # Load json object and print it out
  json_record = json.loads(line)
  print(json_record)


def make_column_args(json):
    '''takes a dict keys as a row, and returns a string of the
    column names and datatypes (assumes all text) to use in creating
    a new table'''
    header = json.keys()
    header = ['"' + i + '"' for i in header]  # quotes allow for columns starting with numbers
    columns = ','.join(header)
    columns = '(' + columns + ' )'
    return columns


def post_row(conn, tablename, rec, columns):
    conn.execute('''CREATE TABLE IF NOT EXISTS %s %s''' % (tablename, columns))
    keys = ','.join(rec.keys())
    question_marks = ','.join(list(['?'] * len(rec)))
    lite = []

    for key, value in rec.items():  # transfer the all the values into string type
        lite.append(str(value))

    values = tuple(lite)
    conn.execute('INSERT INTO ' + tablename + ' (' + keys + ') VALUES (' + question_marks + ')', values)
    conn.commit()
    conn.close()


columns = make_column_args(json_record)
post_row(conn, 'my_table', json_record,columns)
