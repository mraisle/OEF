#preloop activity 

library(rvest)
library(stringr)
library(xml2)
library(anytime)
library(dplyr)

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

together %>% 
  mutate(MainAuthor = case_when(str_detect(author, "Adam Wagner" ) ~ "Adam Wagner",
                                str_detect(author, "Liz McLaughlin" ) ~ "Liz McLaughlin",
                                str_detect(author, "John Deem" ) ~ "John Deem",
                                str_detect(author, "Gareth McGrath" ) ~ "Gareth McGrath",
                                str_detect(author, "David Boraks" ) ~ "David Boraks",
                                str_detect(author, "Emily Jones" ) ~ "Emily Jones",
                                str_detect(author, "Drew Kann" ) ~ "Drew Kann",
                                str_detect(author, "Marisa Mecke" ) ~ "Marisa Mecke"))

together$links <- paste0("<a href='",together$links,"'>","Link to Article","</a>")
colnames(together) <- c("Article Title", "Key Author", "Date Published", "News Outlet","Preview","Link","All Authors")
together$`Date Published` <- anydate(together$`Date Published`)


#this is wrong look at bottom of testing special files for better here
together <- together[, c(7, 1,2,3,4,5,6)]





save(together, file="cleandataset.rda")

#ok, so this works except for files where the outlet is listed incorrectly, have to pull those out separetly


#ADD the target blank to the html links to make them open in new tab

data2 <- data
data2$Link <- sub(">", " target=\"_blank\">", data2$Link, fixed = TRUE)

write.csv(data, "fulldata_3.csv")
data <- read.csv("fulldata_3.csv")
data$ID <- 1:nrow(data)

df1$consecutive_numbers<-1:nrow(df1)