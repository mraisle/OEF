#Trying to using an API in R!

library(httr)
library(jsonlite)

#specify API's url
call <- "http://www.omdbapi.com/?i=tt3896198&apikey=948d3551&plot=full&r=json"
get_movie_details <- GET(url = call)
#status code included above (and below). is status: 200 = successful

# Getting status of HTTP Call
status_code(get_movie_details)

# Content in the API
str(content(get_movie_details))

# Converting content to text
get_movie_text <- content(get_movie_details,
                          "text", encoding = "UTF-8")
get_movie_text

# Parsing data in JSON
get_movie_json <- fromJSON(get_movie_text,
                           flatten = TRUE)
get_movie_json

# Converting into dataframe
get_movie_dataframe <- as.data.frame(get_movie_json)

#OK NEWS API IS OUT - ONLY NATIONAL SOURCES

newskey <- "7da9827e49434edc9219bbffbb6e93c4"

testnews <- GET("https://newsapi.org/v2/everything?q=Apple&from=2022-09-15&sortBy=popularity&apiKey=7da9827e49434edc9219bbffbb6e93c4"
)
status_code(testnews)


sources <- GET("https://newsapi.org/v2/top-headlines/sources",
                query=list(apiKey=newskey,language="en",country="us"))



data = fromJSON(rawToChar(sources$content))
test2 <- as.data.frame(data)

raleightest <- GET("https://newsdata.io/api/1/news",
               query=list(apiKey=IOkey,language="en",country="us",domain="ajc"))


raleightest

#ok io no good - can't find any of the necessary domains 
