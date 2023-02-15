#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 15 14:02:59 2023

Scrape Wikipedia Pages for City Population Data

@author: ozgurseker
"""

import string
import pandas as pd
import wikipedia as wp
import time
import locale

locale.setlocale(category=locale.LC_ALL,
                 locale="Turkish")
wp.set_lang("tr")


scraped_tables = []
years = [1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 2000, 2007]
for i in range(2008, 2023):
    years.append(i)
    
suffixPre2000 = "_Türkiye_nüfus_sayımı"
suffixPost2000 = "_Türkiye_adrese_dayalı_nüfus_kayıt_sistemi_sonuçları"

ncols = [5,6]

newcolnames = ["rank", "city","total", "urban","rate","rural"]

for year in years:
    print(year)
    ncol = ncols[1]
    newcolname = newcolnames
    if year < 1965:
        ncol = ncols[0]
        newcolname = newcolnames[0:4] + newcolnames[5:6]
        
    if year < 2001:
        suffix = suffixPre2000
    else:
        suffix = suffixPost2000
        
    html = wp.page(str(year)+suffix).html().encode("UTF-8")
    df = pd.read_html(html)
    print([df[i].shape[1] for i in range(len(df))])
    tableindex = [df[i].shape[1] for i in range(len(df))].index(ncol)
    dft = df[tableindex]
    dft.columns = newcolname
    dft["year"] = year
    scraped_tables.append(dft)
    time.sleep(1)

dfall = pd.concat(scraped_tables)
dfall.to_csv("turkey_cities.csv", index=False)

dfall["city"].str.replace("Ä±", "ı")
