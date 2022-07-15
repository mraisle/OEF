

library(ggplot2)
library(dplyr)
library(here)
library(stringr)
library(tidyr)


#load csv

individuals_raw <- read.csv(here("individuals_raw_GA.csv"))

favnews_raw <- individuals_raw %>%
  select(1,8,10,15,16,17,38,39,40)


#Atlanta DMA rule
#NEED TO DO
#also need to make function saying if not DMA name in georgia than say NA
# need to manuall edit: 13wmaz and fox5 news; ABC  channel 5,2; Rock 100.5; My favorite local news outlet is News Channel 3 in Memphis; 
# News channel 9 (no DMA); Whnt 19 (hunstville); Wmur channel 9 (savannah); Abc/good morning america; News24; ABC, Channel 2; Fox 6

#next time, sort by DMA first and then apply rules?

#If list multiple --> NA
#included AJC as AJC
#all radio to RADIO
#if not in GA --> NA
#newspaper --> NEWSPAPER
#151 NAs, you had 105




WXIA <- c("11alive","alive","Alive","11 alive","11 Alive","Channel 11","11","channel 11")
WSB <- c("WSB", "Wsb","wsb", "WSB-TV","wsb-tv","Wsb-tv","Ch. 2", "Channel 2", "channel 2", "2")
WTOCSav <- c("WTOC", "wtoc", "Wtoc")
WRDW12 <- c("WRDW", "wrdw", "Wrdw", "WRDW 12", "WRDW 26", "Channel 12", "Channel 26", "Ch 12", "Ch 26",
            "channel12", "channel26")
WJBFAugusta <- c("WJBF", "wjbf", "Wjbf", "New Channel 6", "Channel 6", "channel 6", "Ch 6", "Ch. 6", "Wjbf 6")
FOXlocal <- c("fox5", "FOX 5", "fox 5","Fox 5", "FOX5", "Channel 5", "channel 5", "5", "channel5", "Channel5", "WAGA")
NBCAlbany <- c("WALB", "walb", "Walb", "Channel 10", "channel 10", "Ch 10", "WALB 10", "Walb 10", "walb 10")
NBClocal <- c("WLBT", "wlbt", "WLBT11","wlbt11", "wlbt 11","WLBT 11", "Channel 3", "channel 3", "Ch 3", "channel3", "Channel3")
#NEED TO FIX THIS LATER
CBSlocalmacon <- c("WMAZ", "Wmaz", "wmaz", "13WMAZ", "WMAZ13", "13wmaz","wmaz13", "wmaz 13","WMAZ 13", "Channel 13", "channel 13", "Ch 13", "channel13", "Channel13", "13")
ABClocalcolumbus <- c("WTVM", "Wtvm","wtvm", "9WTVM", "wtvm","wtvm9", "WTVM 9", "wtvm 9","Wtvm 9", "Channel 9", "channel 9", "Ch 9", "channel9", "Channel9", "9", "News leader nine")
CBSlocalatlanta <- c("CBS 46", "cbs 46", "Cbs 46", "WGCL", "Wgcl-TV", "wgcl", "Ch 46", "Channel 46", "channel 46", "46", "channel46", "Channel46", "wgcl 46",
                     "WGCL 46", "Wgcl 46", "Wgcl")
FOX <- c("Fox", "fox", "FOX", "Fox News", "fox news", "Fox news", "FOX News", "Fox and friends")
A <- c("Abc", "ABC", "abc","ABC tv news", "Abc news", "ABC News", "ABC news")
C <- c("CBS", "cbs", "Cbs")
CN <- c("CNN", "cnn", "Cnn", "CNN 10", "CNN10")
M <- c("MSNBC", "msnbc", "Msnbc")
N <- c("NBC", "Nbc", "nbc", "nbc news", "NBC nightly news")
G <- c("Google", "google", "GOOGLE", "Google News", "google news")
SM <- c("facebook", "Facebook", "FACEBOOK", "Fb", "Twitter", "twitter", "instagram", "Instagram", "tik tok",
        "social media", "Social Media", "Youtube", "YouTube")

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
#nbc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(NatlNBC = What.is.your.favorite.local.news.outlet. %in% N)
favnews_raw$NatlNBC<-as.integer(favnews_raw$NatlNBC)



  #works for exact
  #look <- favnews_raw %>% mutate(is.fruit = What.is.your.favorite.local.news.outlet. %in% WXIA)
  
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
    mutate(WMAZ = sum(str_detect(What.is.your.favorite.local.news.outlet., CBSlocalmacon)))
  #WTVM
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WTVM = sum(str_detect(What.is.your.favorite.local.news.outlet., ABClocalcolumbus)))
  #WTOC
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WTOC = sum(str_detect(What.is.your.favorite.local.news.outlet., WTOCSav)))
  #WGCL
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WGCL = sum(str_detect(What.is.your.favorite.local.news.outlet., CBSlocalatlanta)))
  #WSBTV
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WSBTV = sum(str_detect(What.is.your.favorite.local.news.outlet., WSB)))
  #WRDW
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WRDW = sum(str_detect(What.is.your.favorite.local.news.outlet., WRDW12)))
  #WJBF
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WJBF = sum(str_detect(What.is.your.favorite.local.news.outlet., WJBFAugusta)))
  #WALB
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WALB = sum(str_detect(What.is.your.favorite.local.news.outlet., NBCAlbany)))
  #GOOGLE
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(GOOGLE = sum(str_detect(What.is.your.favorite.local.news.outlet., G)))
  #SOCIAL MEDIA
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(SOCIALMEDIA = sum(str_detect(What.is.your.favorite.local.news.outlet., SM)))
  
  
  
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
      } else if(WGCL > 0) {
        "WGCL"
      } else if(WTVM > 0) {
        "WTVM"
      } else if(WTOC > 0) {
        "WTOC"
      } else if(WSBTV > 0) {
        "WSBTV"
      } else if(WRDW > 0) {
        "WRDW"
      } else if(WJBF > 0) {
        "WJBF"
      } else if(WALB > 0) {
        "WALB"
      } else if(GOOGLE > 0) {
        "GOOGLE"
      } else if(SOCIALMEDIA > 0) {
        "SOCIAL MEDIA"
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
      } else if(NatlNBC > 0) {
        "NBC"
      } else {
        "other"
      }
    )
      
      
 favnews_raw <- favnews_raw[, c(1,2,3,4,5,23,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]  
    
  
#test to see how many
  #look2[look2$is.fruit > 0, ] 
  table(look2$is.fruit > 0)
  table(look2$is.fruit > 0)
  table(favnews_raw$FavNewsClean)["WXIA"]
  table(favnews_raw$FavNewsClean)["ABC"]
  
  test <- favnews_raw[favnews_raw$FOX,TRUE, ] 
  look <- favnews_raw[favnews_raw$FavNewsClean == "WTVM",]
  look <- favnews_raw[favnews_raw$SOCIALMEDIA > 0,]
  look <- favnews_raw[favnews_raw$FavNewsClean == "other",]
  
  #save
  write.csv(favnews_raw, here("favnews_routput_GA.csv"))

  #read back in updated version after deal with NAs in excel
  update_fav <- read.csv(here("favnews_routput_GA.csv"))
  unique(update_fav$FavNewsClean)
  
  sum(is.na(update_fav$FavNewsClean))
  
  update_fav$FavNewsClean <- as.factor(update_fav$FavNewsClean)
  
  #summarise by station name
  
  
  try3 <- as.data.frame(table(update_fav$FavNewsClean))
  
  write.csv(try3,here("stationsummary_GA.csv"))

#Frequency Table
#count total for each 
  
#remove DMAs out of Georgia 
  HearCC <- individuals_raw %>%
    select(17,39)
  
  outmarkets <-  paste(c("Chattanooga","Jacksonville","Greenvll-Spart-Ashevll-And"), collapse = '|')
  GAonly <-  HearCC %>% filter(!grepl(outmarkets, DMA.Name)) 
  
  
  GAonly["DMA.Name"][GAonly["DMA.Name"] == ""] <- "Unknown"

GAonly %>%
  distinct(How.often.do.you.hear.about.climate.change.in.the.media., DMA.Name) %>%
  group_by(DMA.Name) %>%
  summarize("CC Frequency per DMA" = n())

#generate table
GAsummary <- pivot_wider(GAonly, names_from = DMA.Name, values_from = How.often.do.you.hear.about.climate.change.in.the.media.)

try2<- GAonly %>% 
  group_by(DMA.Name) %>% 
  summarise(Monthly= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month"), 
            SeveralYear= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year"), 
            Yearorless= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less"), 
            OnceWeek= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week"),
            Never= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Never"))

#switch r<>c
df_transpose = t(try2)
colnames(df_transpose) <- df_transpose [1,]
df_transpose  <- df_transpose [-1, ] 

#convert back to df
df_transpose$`Albany, GA` <- as.data.frame(df_transpose)

df_transpose$`Albany, GA` <- as.numeric(df_transpose$`Albany, GA`)
df_transpose$Atlanta  <- as.numeric(df_transpose$Atlanta )
df_transpose$'Augusta-Aiken'   <- as.numeric(df_transpose$'Augusta-Aiken'  )
df_transpose$'Columbus, GA (Opelika, AL)'  <- as.numeric(df_transpose$'Columbus, GA (Opelika, AL)'  )
df_transpose$Macon  <- as.numeric(df_transpose$Macon )
df_transpose$Savannah   <- as.numeric(df_transpose$Savannah  )
df_transpose$'Tallahassee-Thomasville'  <- as.numeric(df_transpose$'Tallahassee-Thomasville' )
df_transpose$Unknown   <- as.numeric(df_transpose$Unknown  )

#create summary row and column
df_transpose$Georgia = rowSums(df_transpose[,c(1,2,3,4,5,6,7,8)])
df_transpose[nrow(df_transpose) + 1,] = colSums(df_transpose)
row.names(df_transpose)[6] <- "Total"

df_transpose$Georgia = rowSums(df_transpose[,c(-1)])

#save
write.csv(df_transpose,here("frequency_count_GA.csv"))

