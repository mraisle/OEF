#now let's try the scraping way

library(rvest)
library(stringr)


#get title 
liz <- read_html("muckrack/emily_jones.html")
title <- liz %>% html_nodes(".news-story-title") %>% html_text
title <- as.data.frame(title)


 #get html links 
links <- liz %>% html_nodes(".news-story-title") %>% 
  html_elements("a") %>%
  html_attr("href") 
link <- as.data.frame(links)


#get author
author <- liz %>% html_nodes(".mr-byline") %>% html_text
author <- as.data.frame(author)

#get time
time <- liz %>% html_nodes(".timeago") %>% html_text
time <- as.data.frame(time)

#get outlet
source <- liz %>% 
  html_nodes(".news-story-byline") %>% 
  html_elements("a") %>%
  html_attr("href") 
source
outlet <- str_subset(source, "/media-outlet")
outlet

#ok so this works below, now just need to remove /media-outlet/
#remove "/media-outlet/"
outlet <- str_remove(outlet, "/media-outlet/")
outlet <- str_remove(outlet, "/")

#get summary
summary <- liz %>% 
  html_nodes(".news-story-body")
toremove <- summary %>%
  html_nodes(".news-story-byline") 
xml_remove(toremove)

#!!!!!remove nodes - this removes the news-story-byline node from the entire html loaded
#it doesn't destroy the local copy, but to get this node back need to clear environment and reload
xml_remove(toremove)

#convert the parent nodes to text
summary <- summary %>% html_text(trim=TRUE)
summary <- as.data.frame(summary)


emily_jones<- cbind(title,author,time, outlet,link,summary)



#we would want to have this at the very end of the loop, maybe just make a loop for each author and then 
#combine by hand?
all <- rbind(john,adam_wag, liz_m,gareth_m, david_b,marisa_m, emily_jones)
colnames(all)[1] <- "Title"

write.csv(all, "articlesummary.csv")

#END HERE





#aside

source <- as.data.frame(source)
# for drew source <- source[-c(37,41,45,53,54,57,62,66,70,74,78,82,86),]
source <- source[-c(6,10,14,27,31,35,39,43,47,51),]

newrow <- c("The Atlanta Journal-Constitution")
short <- source[c(1:14)]
short <- as.data.frame(short)
short <- rbind(short,newrow)

long <- source[c(15:74)]
long <- as.data.frame(long)
colnames(long) <- c("short")
source <- rbind(short,long)
colnames(source) <- c("source")
source <- as.character(source)

#OLD strategy used to get the time,author, outlet, it relied on the place of these items in a larger class,
#resulted in missteps when multiple authors on a pub 
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
