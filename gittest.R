

library(ggplot2)
library(dplyr)
library(here)
library(stringr)

#load csv

individuals_raw <- read.csv(here("individuals_raw_GA.csv"))

favnews_raw <- individuals_raw %>%
  select(1,8,10,15,16,17,38,39,40)


#Atlanta DMA rule
#NEED TO DO
#also need to make function saying if not DMA name in georgia than say NA
# need to manuall edit: 13wmaz and fox5 news; ABC  channel 5,2; Rock 100.5, My favorite local news outlet is News Channel 3 in Memphis.




WXIA <- c("11alive","alive","Alive","11 alive","11 Alive","Channel 11","11","channel 11")
FOXlocal <- c("fox5", "FOX 5", "fox 5","Fox 5", "FOX5", "Channel 5", "channel 5", "5", "channel5", "Channel5")
NBClocal <- c("WLBT", "wlbt", "WLBT11","wlbt11", "wlbt 11","WLBT 11", "Channel 3", "channel 3", "Ch 3", "channel3", "Channel3")
CBSlocal <- c("WMAZ", "wmaz", "13WMAZ", "WMAZ13", "13wmaz","wmaz13", "wmaz 13","WMAZ 13", "Channel 13", "channel 13", "Ch 13", "channel13", "Channel13", "13")
Coxlocal <- c("WSOC-TV", "wsoc-tv", "9WSOCTV", "WSOC", "wsoc","wsoc9", "WSOC 9", "wsoc 9","WSOC-TV 9", "Channel 9", "channel 9", "Ch 9", "channel9", "Channel9", "9")
FOX <- c("Fox", "fox", "FOX")
A <- c("Abc", "ABC")
C <- c("CBS", "cbs")
CN <- c("CNN", "cnn")
M <- c("MSNBC", "msnbc")

#let's do exact ones
#Fox
favnews_raw %<>% 
  rowwise() %>% 
  mutate(FOXNEWS = What.is.your.favorite.local.news.outlet. %in% FOX)
favnews_raw$FOXNEWS <-as.integer(favnews_raw$FOXNEWS)

#Abc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(ABC = What.is.your.favorite.local.news.outlet. %in% A)
favnews_raw$ABC <-as.integer(favnews_raw$ABC)

#cbs
favnews_raw %<>% 
  rowwise() %>% 
  mutate(CBS = What.is.your.favorite.local.news.outlet. %in% C)
favnews_raw$CBS <-as.integer(favnews_raw$CBS)

#cnn
favnews_raw %<>% 
  rowwise() %>% 
  mutate(CNN = What.is.your.favorite.local.news.outlet. %in% CN)
favnews_raw$CNN <-as.integer(favnews_raw$CNN)

#msnbc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(MSNBC = What.is.your.favorite.local.news.outlet. %in% M)
favnews_raw$MSNBC<-as.integer(favnews_raw$MSNBC)


  #works for exact
  look <- favnews_raw %>% mutate(is.fruit = What.is.your.favorite.local.news.outlet. %in% WXIA)
  
  #works for nuance

  #WXIA
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WXIA = sum(str_detect(What.is.your.favorite.local.news.outlet., WXIA)))
  #FOX5
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(FOX5 = sum(str_detect(What.is.your.favorite.local.news.outlet., FOXlocal)))
  #WLBT
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WLBT = sum(str_detect(What.is.your.favorite.local.news.outlet., NBClocal)))
  #WMAZ
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WMAZ = sum(str_detect(What.is.your.favorite.local.news.outlet., CBSlocal)))
  #WSOC-TV
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WSOCTV = sum(str_detect(What.is.your.favorite.local.news.outlet., Coxlocal)))
  #WSOC-TV
  
#combine to one column 
  
 favnews_raw <- favnews_raw %>%
    mutate(
      FavNewsClean = if(WXIA > 0) {
        "WXIA"
      } else if(FOX5 > 0) {
        "FOX5"
      } else if(WLBT > 0) {
        "WLBT"
      } else if(WMAZ > 0) {
        "WMAZ"
      } else if(FOXNEWS > 0) {
        "FOX"
      } else if(ABC > 0) {
        "ABC"
      } else if(CBS > 0) {
        "CBS"
      } else if(CNN > 0) {
        "CNN"
      } else if(MSNBC > 0) {
        "MSNBC"
      } else {
        "other"
      }
    )
      
      
 favnews_raw <- favnews_raw[, c(1,2,3,4,5,12,6,7,8,9,10,11,13,14,15,16)]  
    
  
#test to see how many
  #look2[look2$is.fruit > 0, ] 
  table(look2$is.fruit > 0)
  table(look2$is.fruit > 0)
  table(favnews_raw$FavNewsClean)["WXIA"]
  table(favnews_raw$FavNewsClean)["ABC"]
  
  test <- favnews_raw[favnews_raw$FOX,TRUE, ] 
  look <- favnews_raw[favnews_raw$FavNewsClean == "WMAZ",]
  look <- favnews_raw[favnews_raw$WSOCTV > 0,]
  