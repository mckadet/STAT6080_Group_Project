library(httr)
library(rvest)
library(tidyverse)

url <- "https://weekly-ads.us/archive/commissary"
ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/12.1.1 Safari/605.1.15"
root_response <- session(url, user_agent(ua))
root_status <- http_status(root_response)
root_content <- content(root_response)
root_content
x <- read_html(root_response)
