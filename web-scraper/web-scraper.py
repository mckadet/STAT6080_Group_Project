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
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from re import search
from time import sleep
from random import random
import file
import urllib.request

url = "https://weekly-ads.us/archive/commissary"

driver = webdriver.Chrome() # link to web driver connected to chromium
driver.get(url)  # access site
links = driver.find_elements_by_tag_name('a')
link_urls = [] # initialize list of urls to target
regex = "^\d{2}-\d{4}$" # regex pattern for mm-yyyy date format

for link in links: # check each link in list of all links
    if bool(search(regex, link.text)): # If link follows date format
        link_urls.append(link.get_attribute('href')) # Then add url to url list



driver2 = webdriver.Chrome()
driver2.get(link_urls[1])
ad_links = driver2.find_elements_by_xpath("//div[@class = 'leaflet-detail']/a[starts-with(@href, '/commissary-ads/flyer-')][1]")

for link in ad_links:
    link.get_attribute('href') 

# Single Ad Multiple Pages for each page
driver3 = webdriver.Chrome()
driver3.maximize_window()
image_urls = []

for link in ad_links:
    driver3.implicitly_wait(0.5)
    driver3.get(link.get_attribute('href'))
    driver3.implicitly_wait(0.5)
    image_urls.append(driver3.find_element_by_id("leaflet").get_attribute('src'))
