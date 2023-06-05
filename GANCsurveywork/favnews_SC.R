

library(ggplot2)
library(dplyr)
library(here)
library(stringr)
library(tidyr)
library(tidyverse)
library(reshape2)


#load csv

individuals_raw <- read.csv("individualsraw_SC_May2023.csv")

favnews_raw <- individuals_raw %>%
  select(1,8,10,15,16,17,38,39,40)
#Get rid of any NA values 
favnews_raw[is.na(favnews_raw)] <- ""


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




WYFF <- c("wyff", "WYFF", "Wyff", "Channel 4", "channel 4", "4","WYFF4", "Ch 4", "channel4", "Channel4",
          "WYFF 4", "wyff4")
FoxCarolina <- c("Fox Carolina", "WHNS", "Fox carolina", "WHNS", "Fox 21")
FOXAtlanta <- c("fox5", "FOX 5", "fox 5","Fox 5", "FOX5","WAGA")

HolyCitySinner <- c("Holy city sinner", "Holy City Sinner", "HCS")
WSPA <- c("WSPA", "News7", "News 7", "7", "Channel 7", "channel 7", "wspa", "Wspa") 
WCSC <- c("WCSC", "Live 5", "WCSC Live5", "wcsc", "wspca", "5 news", "Wcsc")  
TheState <- c("The State", "the state", "state", "State", 	
              "The State newspaper")
WISTV <- c("WISTV", "wistv", "wis-tv", "WIS-TV", "Wistv", "Wis-tv", "Wis-TV", "channel 10",
           "Channel 10", "10", "channel10", "Channel10", "Ch 10", "ch 10", "wis tv", "WIS TV")
TheLink <- c("The Link", "The 'link' newspaper", "the link")
WSOCTV <- c("WSOC", "WSOCTV", "wsoc", "wsoctv", "channel 9", "Channel 9", "Channel 9 w S O c",
            "WSOC TV channel 9")
WLTX <- c("Wltx", "WLTX", "wltx", "Channel 19", "channel 19", "Ch 19", "ch 19")
WCBD <- c("WCBD", "Channel 2", "wcbd", "Wcbd", "channel 2", "News2")
WRDW12 <- c("WRDW", "wrdw", "Wrdw", "WRDW 12", "WRDW 26", "Channel 12", "Channel 26", "Ch 12", "Ch 26",
            "channel12", "channel26", "Wdrw news 12")
NewsBreak <- c("Newsbreak", "News Break", "News break")
WFAE <- c("WFAE", "wfae")
WCCB <- c("WCCB", "wccb", "WCCB news")
WJCL <- c("WJCL", "wjcl", "channel 22 abc")
FOX <- c("Fox", "fox", "FOX", "Fox News", "fox news", "Fox news", "FOX News", "Fox and friends")
NYT <- c("The New York Times", "New York Times", "The new york times", "new york times")
A <- c("Abc", "ABC", "abc","ABC tv news", "Abc news", "ABC News", "ABC news")
C <- c("CBS", "cbs", "Cbs", "Cbs News", "CBS News")
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
  mutate(FOXNEWS = What.is.your.favorite..local..news.outlet. %in% FOX)
favnews_raw$FOXNEWS <-as.integer(favnews_raw$FOXNEWS)

#NYT
favnews_raw %<>% 
  rowwise() %>% 
  mutate(NYT = What.is.your.favorite..local..news.outlet. %in% NYT)
favnews_raw$NYT <-as.integer(favnews_raw$NYT)

#Abc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(ABC = What.is.your.favorite..local..news.outlet. %in% A)
favnews_raw$ABC <-as.integer(favnews_raw$ABC)

#cbs
favnews_raw %<>% 
  rowwise() %>% 
  mutate(CBS = What.is.your.favorite..local..news.outlet. %in% C)
favnews_raw$CBS <-as.integer(favnews_raw$CBS)

#cnn
favnews_raw %<>% 
  rowwise() %>% 
  mutate(CNN = What.is.your.favorite..local..news.outlet. %in% CN)
favnews_raw$CNN <-as.integer(favnews_raw$CNN)

#msnbc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(MSNBC = What.is.your.favorite..local..news.outlet. %in% M)
favnews_raw$MSNBC<-as.integer(favnews_raw$MSNBC)
#nbc
favnews_raw %<>% 
  rowwise() %>% 
  mutate(NatlNBC = What.is.your.favorite..local..news.outlet. %in% N)
favnews_raw$NatlNBC<-as.integer(favnews_raw$NatlNBC)



  #works for exact
  #look <- favnews_raw %>% mutate(is.fruit = What.is.your.favorite..local..news.outlet. %in% WXIA)
  
  #works for nuance

  #WYFF
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WYFF = sum(str_detect(What.is.your.favorite..local..news.outlet., WYFF)))
  #FOXAtlanta
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(FOXAtlanta = sum(str_detect(What.is.your.favorite..local..news.outlet., FOXAtlanta)))
  #FoxCarolina
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(FoxCarolina = sum(str_detect(What.is.your.favorite..local..news.outlet., FoxCarolina)))
  #HolyCitySinner
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(HolyCitySinner = sum(str_detect(What.is.your.favorite..local..news.outlet., HolyCitySinner)))
  #WCSC 
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WCSC = sum(str_detect(What.is.your.favorite..local..news.outlet., WCSC)))
  #TheState
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(TheState = sum(str_detect(What.is.your.favorite..local..news.outlet., TheState)))
  #WISTV
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WISTV = sum(str_detect(What.is.your.favorite..local..news.outlet., WISTV)))
  #WRDW
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WRDW = sum(str_detect(What.is.your.favorite..local..news.outlet., WRDW12)))
  #TheLink
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(TheLink = sum(str_detect(What.is.your.favorite..local..news.outlet., TheLink)))
  #WSOCTV
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WSOCTV = sum(str_detect(What.is.your.favorite..local..news.outlet., WSOCTV)))
  #WLTX
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WLTX = sum(str_detect(What.is.your.favorite..local..news.outlet., WLTX)))
  #WCBD 
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WCBD = sum(str_detect(What.is.your.favorite..local..news.outlet., WCBD)))
  #NewsBreak 
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(NewsBreak = sum(str_detect(What.is.your.favorite..local..news.outlet., NewsBreak)))
  #WFAE
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WFAE = sum(str_detect(What.is.your.favorite..local..news.outlet., WFAE)))
  #WCCB
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WCCB = sum(str_detect(What.is.your.favorite..local..news.outlet., WCCB)))
  #WJCL
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WJCL = sum(str_detect(What.is.your.favorite..local..news.outlet., WJCL)))
  #WSPA 
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(WSPA  = sum(str_detect(What.is.your.favorite..local..news.outlet., WSPA)))
  #GOOGLE
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(GOOGLE = sum(str_detect(What.is.your.favorite..local..news.outlet., G)))
  #SOCIAL MEDIA
  favnews_raw %<>% 
    rowwise() %>% 
    mutate(SOCIALMEDIA = sum(str_detect(What.is.your.favorite..local..news.outlet., SM)))
  

  
#combine to one column 
  
 favnews_raw <- favnews_raw %>%
    mutate(
      FavNewsClean = if(WYFF > 0) {
        "WYFF"
      } else if(FOXAtlanta > 0) {
        "FOXAtlanta"
      } else if(FoxCarolina > 0) {
        "FoxCarolina"
      } else if(HolyCitySinner > 0) {
        "HolyCitySinner"
      } else if(WSPA  > 0) {
        "WSPA"
      } else if(TheState  > 0) {
        "TheState"
      } else if(WISTV > 0) {
        "WISTV"
      } else if(WRDW > 0) {
        "WRDW"
      } else if(TheLink > 0) {
        "TheLink"
      } else if(WSOCTV > 0) {
        "WSOCTV"
      } else if(WLTX > 0) {
        "WLTX"
      } else if(WCBD  > 0) {
        "WCBD"
      } else if(NewsBreak  > 0) {
        "NewsBreak"
      } else if(WFAE > 0) {
        "WFAE"
      } else if(WCCB > 0) {
        "WCCB"
      } else if(WJCL > 0) {
        "WJCL"
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
      
      
 favnews_raw2 <- favnews_raw[, c(1,2,3,4,36,5,6,7,8)]  
    
  
#test to see how many
  #look2[look2$is.fruit > 0, ] 
  table(look2$is.fruit > 0)
  table(look2$is.fruit > 0)
  table(favnews_raw$FavNewsClean)["WYFF"]
  table(favnews_raw$FavNewsClean)["other"]
  
  test <- favnews_raw[favnews_raw$FOX,TRUE, ] 
  look <- favnews_raw[favnews_raw$FavNewsClean == "WTVM",]
  look <- favnews_raw[favnews_raw$SOCIALMEDIA > 0,]
  look <- favnews_raw[favnews_raw$FavNewsClean == "other",]
  
  #save
  write.csv(favnews_raw2, "favnews_routput_SC.csv")

  #read back in updated version after deal with NAs in excel
  update_fav <- read.csv("favnews_routput_SC.csv")
  unique(update_fav$FavNewsClean)
  
  sum(is.na(update_fav$FavNewsClean))
  
  update_fav$FavNewsClean <- as.factor(update_fav$FavNewsClean)
  
  #summarise by station name
  
  
  try3 <- as.data.frame(table(update_fav$FavNewsClean))
 try2 <-  try3[order(-try3$Freq),]
  
  
  write.csv(try2,"stationsummary_SC.csv")

#Count most popular local news in each DMA
  update_fav2 <- update_fav[, c(6,10)]  
  update_fav2$DMA.Name <- as.factor(update_fav2$DMA.Name)
  count_DMAs <- table(update_fav2$FavNewsClean, update_fav2$DMA.Name)
  count_df <- as.data.frame.matrix(count_DMAs)
  
  write.csv(count_df,"DMAsummary_SC.csv")
  
  

#THIS WORKS
try4<- update_fav2 %>% 
  group_by(DMA.Name) %>% 
  summarise(Monthly= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month"), 
            SeveralYear= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year"), 
            Yearorless= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less"), 
            OnceWeek= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week"),
            Never= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Never"))


#TRYING TO DO WEIGHTED SUM 
weightattempt1 <- individuals_raw %>%
  select(17,39,40)

#remove outmarkets (use outmarkets vector from above)
GAweight <-  weightattempt1 %>% filter(!grepl(outmarkets, DMA.Name)) 
GAweight["DMA.Name"][GAweight["DMA.Name"] == ""] <- "Unknown"


weight2<- GAweight %>% 
  group_by(DMA.Name) %>% 
  summarise(Monthly= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month") * Weight ), 
            SeveralYear= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year") * Weight ), 
            Yearorless= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less") * Weight ), 
            OnceWeek= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week") * Weight ),
            Never= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Never") * Weight ))

#switch r<>c
df_transpose = t(try2)
colnames(df_transpose) <- df_transpose [1,]
df_transpose  <- df_transpose [-1, ] 

#weighted version switch r<>c
weight3 = t(weight2)
colnames(weight3) <- weight3 [1,]
weight3  <- weight3 [-1, ] 

#convert back to df
df_transpose <- as.data.frame(df_transpose)

# weighted version convert back to df
weight3 <- as.data.frame(weight3)


df_transpose$`Albany, GA` <- as.numeric(df_transpose$`Albany, GA`)
df_transpose$Atlanta  <- as.numeric(df_transpose$Atlanta )
df_transpose$'Augusta-Aiken'   <- as.numeric(df_transpose$'Augusta-Aiken'  )
df_transpose$'Chattanooga'   <- as.numeric(df_transpose$'Chattanooga'  )
df_transpose$'Columbus, GA (Opelika, AL)'  <- as.numeric(df_transpose$'Columbus, GA (Opelika, AL)'  )
df_transpose$Macon  <- as.numeric(df_transpose$Macon )
df_transpose$Savannah   <- as.numeric(df_transpose$Savannah  )
df_transpose$'Tallahassee-Thomasville'  <- as.numeric(df_transpose$'Tallahassee-Thomasville' )
df_transpose$Unknown   <- as.numeric(df_transpose$Unknown  )

#create summary row and column
df_transpose$Georgia = rowSums(df_transpose[,c(1,2,3,4,5,6,7,8,9)])
df_transpose[nrow(df_transpose) + 1,] = colSums(df_transpose)
row.names(df_transpose)[6] <- "Total"


#weighted version 
weight3$`Albany, GA` <- as.numeric(weight3$`Albany, GA`)
weight3$Atlanta  <- as.numeric(weight3$Atlanta )
weight3$'Augusta-Aiken'   <- as.numeric(weight3$'Augusta-Aiken'  )
weight3$'Chattanooga'   <- as.numeric(weight3$'Chattanooga'  )
weight3$'Columbus, GA (Opelika, AL)'  <- as.numeric(weight3$'Columbus, GA (Opelika, AL)'  )
weight3$Macon  <- as.numeric(weight3$Macon )
weight3$Savannah   <- as.numeric(weight3$Savannah  )
weight3$'Tallahassee-Thomasville'  <- as.numeric(weight3$'Tallahassee-Thomasville' )
weight3$Unknown   <- as.numeric(weight3$Unknown  )

#create summary row and column
weight3$Georgia = rowSums(weight3[,c(1,2,3,4,5,6,7,8,9)])
weight3[nrow(weight3) + 1,] = colSums(weight3)
row.names(weight3)[6] <- "Total"



#save
write.csv(df_transpose,here("frequency_count_GA.csv"))

#save weighted
write.csv(weight3,here("frequency_count_GA_weighted.csv"))


#for outmarkets 
outmarketsonly %>%
  distinct(How.often.do.you.hear.about.climate.change.in.the.media., DMA.Name) %>%
  group_by(DMA.Name) %>%
  summarize("CC Frequency per DMA" = n())

outmarketsum<- outmarketsonly %>% 
  group_by(DMA.Name) %>% 
  summarise(Monthly= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month"), 
            SeveralYear= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year"), 
            Yearorless= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less"), 
            OnceWeek= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week"),
            Never= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Never"))

#switch r<>c
outsum = t(outmarketsum)
colnames(outmarketsum) <- outmarketsum [1,]
outmarketsum  <- outmarketsum [-1, ] 