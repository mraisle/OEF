#testing files

library(dplyr)
library(anytime)


#together <- data.frame()
#get list of the files
#fileNames <- list.files(path = "/Users/meganraisle/Documents/OEF/muckrack", pattern="*.html")

setwd("/Users/meganraisle/Documents/OEF/testing")

html <- read_html("adam_w_page11.html")


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


#filtering based on author

Adam_Wag <- together[grep("Adam Wagner", together$author), ]
Liz_M <- together[grep("Liz McLaughlin", together$author), ]
John_D <- together[grep("John Deem", together$author), ]
gareth_m <- together[grep("Gareth McGrath", together$author), ]
david_b <- together[grep("David Boraks", together$author), ]
emily_j  <- together[grep("Emily Jones", together$author), ]
drew_k <- together[grep("Drew Kann", together$author), ]
marisa <- together[grep("Marisa Mecke", together$author), ]



#ok, we should make an author column that is "main author"

load("cleandataset.rda")
save(together_authors, file="fulldata.rda")
write.csv(together_authors, "fulldata.csv")
together_authors <- read.csv("fulldata.csv")

together_authors <- together_authors %>% subset(select = -c(X))

#CHANGES TO DATAFRAME ONCE THE LOOP HAS RUN 
#make the link clickable in shiny 
together_authors$links <- paste0("<a href='",together_authors$links,"'>","Link to Article","</a>")


#make the authors searchable 
together_authors <- together %>% 
  mutate(MainAuthor = case_when(str_detect(author, "Adam Wagner" ) ~ "Adam Wagner",
                                str_detect(author, "Liz McLaughlin" ) ~ "Liz McLaughlin",
                                str_detect(author, "John Deem" ) ~ "John Deem",
                                str_detect(author, "Gareth McGrath" ) ~ "Gareth McGrath",
                                str_detect(author, "David Boraks" ) ~ "David Boraks",
                                str_detect(author, "Emily Jones" ) ~ "Emily Jones",
                                str_detect(author, "Drew Kann" ) ~ "Drew Kann",
                                str_detect(author, "Marisa Mecke" ) ~ "Marisa Mecke"))

 #this is only the right reorder when already reordered, will need to modify to put into the loop script.                         
together_authors <- together_authors[, c(2,1,4,5,7,6,3)]

#rename columns 
colnames(together_authors) <- c("Article Title", "Key Author", "Date Published", "News Outlet","Preview","Link","All Authors")

#convert time to readable format for shiny 
test <- anydate(together_authors$`Date Published`)

