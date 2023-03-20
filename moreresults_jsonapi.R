#API KEY: AIzaSyB26Mrd8hBSrH6Xf8-hlPoIy8qcJUiFmPM
#search engine id: 54eb2ef59694f4f6f

#how to access the search engine:
#<script async src="https://cse.google.com/cse.js?cx=54eb2ef59694f4f6f">
#</script>
#  <div class="gcse-search"></div>

library(httr)
library(jsonlite)
library(urltools)
library(dplyr)
library(purrr)

api_key <- "AIzaSyB26Mrd8hBSrH6Xf8-hlPoIy8qcJUiFmPM"
cx <- "54eb2ef59694f4f6f"
query <- "liz mclaughlin"

# Function to fetch search results from the Google Custom Search JSON API
fetch_search_results <- function(api_key, cx, query, start = 1) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  response <- GET(base_url, query = list(key = api_key, cx = cx, q = query, start = start))
  content <- content(response, as = "parsed", type = "application/json")
  return(content)
}

# Create a function to extract the relevant data from a search result item
extract_data <- function(item) {
  title <- item$title %||% NA_character_
  url <- item$link %||% NA_character_
  
  publication_date <- NA_character_
  if (!is.null(item$pagemap$newsarticle) && !is.null(item$pagemap$newsarticle[[1]]$datepublished)) {
    publication_date <- item$pagemap$newsarticle[[1]]$datepublished
  } else if (!is.null(item$pagemap$metatags) && !is.null(item$pagemap$metatags[[1]]$`article:published_time`)) {
    publication_date <- item$pagemap$metatags[[1]]$`article:published_time`
  }
  
  description <- NA_character_
  if (!is.null(item$pagemap$metatags) && !is.null(item$pagemap$metatags[[1]]$`og:description`)) {
    description <- item$pagemap$metatags[[1]]$`og:description`
  }
  
  source <- NA
  if (is.character(url)) {
    parsed_url <- url_parse(url)
    source <- parsed_url$domain
  }
  
  return(data.frame(title = title, url = url,  publication_date =  publication_date, source = source, 
                    description = description, stringsAsFactors = FALSE))
}

# Initialize an empty dataframe to store search results
search_results <- data.frame()

# Loop through search results pages and fetch up to 50 results
for (start in seq(70, 120, by = 10)) {
  results <- fetch_search_results(api_key, cx, query, start)
  
  if (length(results$items) == 0) {
    break
  }
  
  # Extract data from search result items and combine them into a single dataframe
  search_results <- bind_rows(search_results, bind_rows(lapply(results$items, extract_data)))
}

# Print the search results dataframe
print(search_results)
