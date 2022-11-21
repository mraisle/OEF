#now let's try the scraping way

library(rvest)


#get title 
liz <- read_html("muckrack/drew_k.html")
liznodes <- liz %>% html_nodes(".news-story-title") %>% html_text
liznodes
title <- as.data.frame(liznodes)


 #get html links 
links <- liz %>% html_nodes(".news-story-title") %>% 
  html_elements("a") %>%
  html_attr("href") 
link <- as.data.frame(links)


#get news outlet, author, time
source <- liz %>% 
  html_nodes(".news-story-byline") %>% 
  html_elements("a") %>%
  html_text()
time <- source[seq(1, length(source), 3)]
time <- as.data.frame(time)
author <- source[seq(2, length(source), 3)]
author <- as.data.frame(author)
outlet <- source[seq(3, length(source), 3)]
outlet <- as.data.frame(outlet)

drew_k<- cbind(title,author,outlet,link)


all <- rbind(john,adam_wag, liz_m,gareth_m, david_b,marisa_m, drew_k)

#END HERE

#this is not working still need to work on this 
summary <- liz %>% 
  html_nodes(".news-story-body.col-12") %>% 
  html_text()

#aside

source <- as.data.frame(source)
source <- source[-c(37,41,45,53,54,57,62,66,70,74,78,82,86),]
newrow <- c("The Atlanta Journal-Constitution")


source <- as.character(source)
