#now let's try the scraping way

library(rvest)


#get title 
liz <- read_html("muckrack/emily_jones.html")
liznodes <- liz %>% html_nodes(".news-story-title") %>% html_text
liznodes
title <- as.data.frame(liznodes)


 #get html links 
links <- liz %>% html_nodes(".news-story-title") %>% 
  html_elements("a") %>%
  html_attr("href") 
link <- as.data.frame(links)


#ok so the links and the title are always correct,
#we have to work on these ones below to make the dataframes come out right 

#ok fixed the author problem here
author2 <- liz %>% html_nodes(".mr-byline") %>% html_text
author2
author2 <- as.data.frame(author2)

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

emily_jones<- cbind(title,author,time, outlet,link)


all <- rbind(john,adam_wag, liz_m,gareth_m, david_b,marisa_m, emily_jones)
colnames(all)[1] <- "Title"

write.csv(all, "articlesummary.csv")

#END HERE

#this is not working still need to work on this 
summary <- liz %>% 
  html_nodes(".news-story-body.col-12") %>% 
  html_text()

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
