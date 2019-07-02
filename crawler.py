# coding: utf-8
from concurrent.futures import ProcessPoolExecutor
from urllib.request import urlretrieve
from urllib.parse import urljoin
from lxml import etree
import requests as rqs
from time import sleep
import itertools
import logging
import os.path
import time
import csv
import os

'''
經濟部商業登記資料查詢爬蟲
'''

def post_arg_generator(area, year):
    '''製造post資訊'''
    return (area, year,)

def save_log():
    '''儲存log檔'''
    pass

def save_pdf(gov_name, text, url_link):
    '''pdf儲存'''
    fname = f'./pdf/{gov_name}{text}.pdf'
    urlretrieve(url_link, filename=fname)

def request_page(post_data, url=URL, timeout=60):
    '''發送請求function'''
    headers = {
        'Connection': 'close',
    }
    trys_limit = 0

    while True: # 防止程式意外錯誤導致抓取資料失敗
        try:
            r = rqs.post(url, data={'area': post_data[0], 'YYYMM': post_data[1],}, headers = headers, timeout=timeout)
            dom = etree.HTML(r.text)
            gov_name = dom.xpath("//td[@class='solid']/text()")[0]
            url_text = dom.xpath("//a/font[1]/text()")
            url_href = [urljoin(PDF_LINK, link) for link in dom.xpath("//td[@class='normal' and 2]/a/@href")]
            logging.warning("%s,%s,%s", post_data[0], post_data[1], r.status_code)
            [save_pdf(gov_name, text, href) for text, href in zip(url_text, url_href)]
        except Exception as err:
            if trys_limit < 100:
                trys_limit += 1
                sleep(8)
                continue
            else:
                logging.error("%s,%s,%s", post_data[0], post_data[1], 'Error')
                break            
        finally:
            break
if __name__ == "__main__":
    os.makedirs('./pdf', exist_ok=True)
    URL = 'https://serv.gcis.nat.gov.tw/moeadsBF/bms/report.jsp?method=first&agencyCode=allbf&showGcisLocation=false&showBusi=true&showFact=false' # 資料首頁連結
    PDF_LINK = 'https://serv.gcis.nat.gov.tw/moeadsBF/bms/' # pdf 下載連結
    r = rqs.get(URL, timeout=10)
    r.encoding = 'big5'
    '''取得post參數'''
    dom = etree.HTML(r.text)
    countrys = dom.xpath('//select[1]/option[position()>1]/@value') # 縣市 header
    years = dom.xpath('//select[2]/option[position()>1]/@value') # 年份 header
    post_args = [post_arg_generator(*arg) for arg in itertools.product(countrys, years)]
    begin = time.time()
    if os.path.isfile('./CrawlerInfo.log'):
        with open('./CrawlerInfo.log', newline='') as f:
            rows = csv.DictReader(f, delimiter=',')
            delpost = [(i['area'], i['YYYMM'],) for i in rows]
            post_args = map(tuple, post_args)
            delpost = map(tuple, delpost)
            post_args = set(post_args)
            delpost = set(delpost)
            print(len(delpost))
            
            
    #     logging.basicConfig(filename='CrawlerInfo.log', filemode='a+', format="%(asctime)s,%(process)d,%(message)s", datefmt='%Y-%m-%d %I:%M:%S %p')
    # else:
    #     logging.basicConfig(filename='CrawlerInfo.log', filemode='a+', format="%(asctime)s,%(process)d,%(message)s", datefmt='%Y-%m-%d %I:%M:%S %p')
    #     with open('./CrawlerInfo.log', 'w') as f:
    #         f.write('time,prossesid,area,YYYMM,status\n')
    # with ProcessPoolExecutor() as executor:
    #     futures = [executor.submit(request_page, post) for post in post_args]
    # print('一共執行：', time.time() - begin, '秒')
