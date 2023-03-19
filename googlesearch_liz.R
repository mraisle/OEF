#testing gpt4 solution 

library(rvest)
library(stringr)
library(tidyverse)

build_advanced_search_url <- function() {
  base_url <- "https://www.google.com/search?q="
  query <- '"liz mclaughlin" "mclaughlin" site:wral.com'
  final_url <- paste0(base_url, URLencode(query))
  return(final_url)
}

final_url <- "https://www.google.com/search?q=%22liz%20mclaughlin%22%20%22mclaughlin%22%20site:wral.com"


scrape_google_search <- function(url, num_results = 100) {
  search_page <- read_html(url)
  
  # Extract search results' titles
  titles <- search_page %>%
    html_nodes(".tF2Cxc .LC20lb.DKV0Md") %>%
    html_text()
  
  # Extract search results' URLs
  urls <- search_page %>%
    html_nodes(".yuRUbf > a") %>%
    html_attr("href")
  
  results <- data.frame(title = titles[1:num_results],
                        url = urls[1:num_results],
                        stringsAsFactors = FALSE)
  
  return(results)
}

url <- build_advanced_search_url()
url <- ("/Users/meganraisle/Documents/OEF/liz mclaughlin _mclaughlin_ site_wral.com - Google Search.html")
results <- scrape_google_search(url)
print(results)


#str_extract("https?://[^/]+")
