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
api_key <- "AIzaSyB26Mrd8hBSrH6Xf8-hlPoIy8qcJUiFmPM"
search_engine_id <- "54eb2ef59694f4f6f"

google_custom_search <- function(query, api_key, search_engine_id) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  response <- GET(base_url,
                  query = list(key = api_key,
                               cx = search_engine_id,
                               q = query))
  parsed_response <- content(response, as = "parsed", type = "application/json")
  return(parsed_response)
}

query <- "liz mclaughlin"
results <- google_custom_search(query, api_key, search_engine_id)

# Extract search result items
items <- results$items

# Create a function to extract the relevant data from a search result item
extract_data <- function(item) {
  title <- item$title
  url <- item$link
  
  publication_date <- NULL
  if (!is.null(item$pagemap$newsarticle)) {
    publication_date <- item$pagemap$newsarticle[[1]]$datepublished
  } else if (!is.null(item$pagemap$metatags)) {
    publication_date <- item$pagemap$metatags[[1]]$`article:published_time`
  }
  
  description <- NULL
  
  if (!is.null(item$pagemap$metatags)) {
    description <- item$pagemap$metatags[[1]]$`og:description`
  }
  
  parsed_url <- url_parse(url)
  source <- parsed_url$domain
  
  return(data.frame(title = title, url = url,  publication_date =  publication_date, source = source, 
                    description = description, stringsAsFactors = FALSE))
}

# Extract data from each search result item and combine them into a single dataframe
search_results <- bind_rows(lapply(items, extract_data))

# Print the search results dataframe
print(search_results)
