#preloop activity 

library(rvest)
library(stringr)
library(xml2)

#make empty dataframe 
together <- data.frame()
#get list of the files
fileNames <- list.files(path = "/Users/meganraisle/Documents/OEF/muckrack", pattern="*.html")

setwd("/Users/meganraisle/Documents/OEF/muckrack")

for (x in fileNames) {
#get title 
html <- read_html(x)
title <- html %>% html_nodes(".news-story-title") %>% html_text
title <- as.data.frame(title)

#get html links 
links <- html %>% html_nodes(".news-story-title") %>% 
  html_elements("a") %>%
  html_attr("href") 
link <- as.data.frame(links)

#get author
author <- html %>% html_nodes(".mr-byline") %>% html_text
author <- as.data.frame(author)

#get time
time <- html %>% html_nodes(".timeago") %>% html_text
time <- as.data.frame(time)

#get outlet
source <- html %>% 
  html_nodes(".news-story-byline") %>% 
  html_elements("a") %>%
  html_attr("href") 
outlet <- str_subset(source, "/media-outlet")

#ok so this works below, now just need to remove /media-outlet/
#remove "/media-outlet/"
outlet <- str_remove(outlet, "/media-outlet/")
outlet <- str_remove(outlet, "/")

#get summary
summary <- html %>% 
  html_nodes(".news-story-body")
toremove <- summary %>%
  html_nodes(".news-story-byline") 

#!!!!!remove nodes - this removes the news-story-byline node from the entire html loaded
#it doesn't destroy the local copy, but to get this node back need to clear environment and reload
xml_remove(toremove)

#convert the parent nodes to text
summary <- summary %>% html_text(trim=TRUE)
summary <- as.data.frame(summary)


current <- cbind(title,author,time, outlet,links,summary)

#this would be the end of the loop

together <- rbind(together, current)
rm(current,links,source,toremove,link, html, outlet, summary, time, title, author)

}

save(together, file="cleandataset.rda")

#ok, so this works except for files where the outlet is listed incorrectly, have to pull those out separetly




