

library(ggplot2)
library(dplyr)
library(here)
library(stringr)

#load csv

individuals_raw <- read.csv(here("individuals_raw_GA.csv"))

favnews_raw <- individuals_raw %>%
  select(1,8,10,15,16,17,38,39,40)


#Atlanta DMA rule
#ok what i want to do = if contains any of the words in the vector, make new column in the row that says "WXIA"
#also need to make function saying if not DMA name in georgia than say NA

WXIA <- c("11alive","alive","Alive","11 alive","11 Alive","Channel 11","11","channel 11")

acs_micro_internet <- acs_micro %>%
  filter(year > 2012) %>%
  mutate(
    MSA = as.character(MSA),
    int_acc = if_else(CINETHH == 1 | CINETHH == 2, T, F),
    int_acc = replace(int_acc, CINETHH == 0, NA),
   
 
  #wors for exact
  look <- favnews_raw %>% mutate(is.fruit = What.is.your.favorite.local.news.outlet. %in% WXIA)
  
  #works for nuance
  
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WXIA = sum(str_detect(What.is.your.favorite.local.news.outlet., WXIA)))
  
  
  favnews_raw <-  favnews_raw %>%
    mutate(
      FavNewsClean = if_else(WXIA > 0, "WXIA","other"))
  
#test to see how many
  #look2[look2$is.fruit > 0, ] 
  table(look2$is.fruit > 0)
  table(favnews_raw$FavNewsClean)["WXIA"]
  