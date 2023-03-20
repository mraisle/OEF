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
date_ranges <- c("2022/06/01:2022/10/31", "2022/11/01:2023/03/19")


# Function to fetch search results from the Google Custom Search JSON API
get_search_results <- function(start_index, api_key, cx, query) {
  url <- "https://www.googleapis.com/customsearch/v1"
  params <- list(
    key = api_key,
    cx = cx,
    q = query,
    num = 10,
    start = start_index,
    sort = "date"
  )
  
  response <- GET(url, query = params)
  if (response$status_code != 200) {
    cat("Error in API request. Status code:", response$status_code, "\n")
    cat("Error message:", content(response, as = "text"), "\n")
    return(NULL)
  }
  
  json <- fromJSON(rawToChar(response$content))
  return(json)
}

# Create a function to extract the relevant data from a search result item
extract_data <- function(all_results) {
  df <- lapply(all_results, function(item) {
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
    
    data.frame(title = title, url = url, publication_date = publication_date, description = description, source = source, stringsAsFactors = FALSE)
  })
  
  df <- do.call(rbind, df)
  return(df)
}


# Retrieve search results for each date range
all_results <- lapply(date_ranges, function(date_range) {
  results <- list()
  
  for (start_index in seq(1, 100, 10)) {
    search_query <- paste(query, "daterange:", date_range, sep = "")
    cat("Retrieving results for date range:", date_range, "and start index:", start_index, "\n")
    json <- get_search_results(start_index, api_key, cx, search_query)
    if (!is.null(json$items)) {
      results <- append(results, json$items)
    }
  }
  
  cat("Number of results for date range", date_range, ":", length(results), "\n")
  return(results)
})

# Flatten the list and remove NULL elements
all_results <- do.call(c, all_results)
all_results <- all_results[!sapply(all_results, is.null)]

# Extract data and create a data frame
results_df <- extract_data(all_results)