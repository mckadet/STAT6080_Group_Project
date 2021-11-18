# -*- coding: utf-8 -*-
"""
Web Scraper developed with Selenium.  Allows python to manipulate Chromium
browser so that all interactions with weekly-ads.us are mediated with an actual
web browser to bypass Cloudflare's DDOS protection system.

Must have Chromium Web Browser (not Google Chrome) installed on system.

Created on Mon Nov  8 12:30:02 2021

@author: Tyler Antoloci
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

url = 'http://www.webscrapingfordatascience.com/complexjavascript/'

driver = webdriver.Chrome() # link to web driver connected to browser

driver.get(url) # access specified url

# driver repeatedly searches for  and selects elements with specified css 
# selector until the presence of all elements have been located or 10s elapses.
quote_elements = WebDriverWait(driver, 10).until(
    EC.presence_of_all_elements_located(
        (By.CSS_SELECTOR, ".quote:not(.decode)")
    )
)

for quote in quote_elements:
    print(quote.text)
    
input('Press ENTER to close the automated browser')
driver.quit()