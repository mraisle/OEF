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
search_engine_id <- "54eb2ef59694f4f6f"

get_search_results <- function(start_index, api_key, cx, query, date_range) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  response <- GET(base_url, query = list(key = api_key, 
                                         cx = search_engine_id, q = query,
                                         start = start_index,
                                         sort = paste("date:r:", date_range, sep = "")))
  content <- content(response, as = "parsed", type = "application/json")
  return(content)
}

query <- "marisa mecke"
date_ranges <- c("20220601:20221231", "20230101:20230423")


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

search_results <- data.frame()


for (date_range in date_ranges) {
  has_more_results <- TRUE
  start_index <- 1
  
  while (has_more_results) {
    json <- get_search_results(start_index, api_key, search_engine_id, query, date_range)
    
    if (!is.null(json$items)) {
      results <- lapply(json$items, extract_data)
      search_results <- bind_rows(search_results, bind_rows(results))
      
      if (length(json$items) < 10) {
        has_more_results <- FALSE
      } else {
        start_index <- start_index + 10
      }
    } else {
      has_more_results <- FALSE
    }
  }
}

# Print the search results dataframe
print(search_results)
Liz <- search_results

#make this data frame align with the existing data file and append 
#data <- read.csv("fulldata_3.csv")

#fix publication Date 
Liz$publication_date <- sub("T.*", "", Liz$publication_date)

#fix URL
link_text <- "Link to Article"
Liz$url <- paste0("<a href='", Liz$url, "' target='_blank'>", link_text, "</a>")

#add Key Author Column 

Liz$Key.Author <- as.character("Marisa Mecke")
Liz$All.Authors <- as.character("")

#remove bad row pulls from google 

clean_Liz <- Liz[!grepl("Marisa Mecke", Liz$title),]




#merge with the data file

colnames(clean_Liz) <- c("Article.Title","Link","Date.Published","News.Outlet", "Preview", "Key.Author", "All.Authors")

write.csv(clean_Liz, "getarticles/marisamecke.csv")


#alltogethernow <- rbind(data, clean_Liz)

#write.csv(alltogethernow, "fulldata_4.csv")
