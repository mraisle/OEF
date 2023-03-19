#testing gpt4 solution 

library(rvest)
library(stringr)
library(tidyverse)

scrape_wral_search_results <- function(url) {
  search_page <- read_html(url)
  
  # Extract article titles
  titles <- search_page %>%
    html_nodes(".queryly_item_title") %>%
    html_text()
  
  # Extract article URLs
  urls <- search_page %>%
    html_nodes(".title a") %>%
    html_attr("href")
  
  # Extract publication dates
  pub_dates <- search_page %>%
    html_nodes(".pub-date") %>%
    html_text()
  
  # Extract article summaries
  summaries <- search_page %>%
    html_nodes(".summary") %>%
    html_text()
  
  # Combine the results into a data frame
  results <- data.frame(title = titles,
                        url = urls,
                        pub_date = pub_dates,
                        summary = summaries,
                        stringsAsFactors = FALSE)
  
  return(results)
}

#url <- "https://www.wral.com/?query=liz%20mclaughlin"
url <- ("/Users/meganraisle/Documents/OEF/wral_liz.html")
results <- scrape_wral_search_results(url)
print(results)

titles <- read_html(url) %>%
  html_nodes(".queryly_item_title") %>%
  html_text()

titles
