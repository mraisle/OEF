

library(ggplot2)
library(dplyr)
library(here)
library(stringr)
library(tidyr)

setwd("/Users/meganraisle/Documents/OEF/GANCsurveywork")

#i've already set up the script this way but next time use the coded variables!!

#load csv

individuals_raw <- read.csv("NC_December2022_surveyraw.csv")

#Frequency Table
#count total for each 
  
#remove DMAs out of Georgia 
  HearCC <- individuals_raw %>%
    select(17,38)
  
  #update, originally removed Chattanooga. Now keeping Chattanooga because >11 records
 # outmarkets <-  paste(c("Jacksonville","Greenvll-Spart-Ashevll-And"), collapse = '|')
  #GAonly <-  HearCC %>% filter(!grepl(outmarkets, DMA.Name)) 
  #outmarketsonly <- HearCC %>% filter(grepl(outmarkets, DMA.Name))
  
  HearCC["DMA.Name"][HearCC["DMA.Name"] == ""] <- "Unknown"

#THIS WORKS
try2<- HearCC %>% 
  group_by(DMA.Name) %>% 
  summarise(Monthly= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month"), 
            SeveralYear= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year"), 
            Yearorless= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less"), 
            OnceWeek= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week"),
            Never= sum(How.often.do.you.hear.about.climate.change.in.the.media.== "Never"))

#add a new column that is the combined value of monthly or once week
try2$Combined = rowSums(try2[,c(2,5)])


#TRYING TO DO WEIGHTED SUM 
#weightattempt1 <- individuals_raw %>%
  #select(17,39,40)

#remove outmarkets (use outmarkets vector from above)
#GAweight <-  weightattempt1 %>% filter(!grepl(outmarkets, DMA.Name)) 
#GAweight["DMA.Name"][GAweight["DMA.Name"] == ""] <- "Unknown"


#weight2<- GAweight %>% 
 # group_by(DMA.Name) %>% 
  #summarise(Monthly= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a month") * Weight ), 
          #  SeveralYear= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Several times a year") * Weight ), 
          #  Yearorless= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Once a year or less") * Weight ), 
           # OnceWeek= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "At least once a week") * Weight ),
           # Never= sum((How.often.do.you.hear.about.climate.change.in.the.media.== "Never") * Weight ))

#switch r<>c
df_transpose = t(try2)
colnames(df_transpose) <- df_transpose [1,]
df_transpose  <- df_transpose [-1, ] 

#weighted version switch r<>c
#weight3 = t(weight2)
#colnames(weight3) <- weight3 [1,]
#weight3  <- weight3 [-1, ] 

#convert back to df
df_transpose <- as.data.frame(df_transpose)

# weighted version convert back to df
#weight3 <- as.data.frame(weight3)


df_transpose <- mutate_all(df_transpose, function(x) as.numeric(as.character(x)))

#^if you do this function above you can skip this messiness below
#df_transpose$`Albany, GA` <- as.numeric(df_transpose$`Albany, GA`)
#df_transpose$Atlanta  <- as.numeric(df_transpose$Atlanta )
#df_transpose$'Augusta-Aiken'   <- as.numeric(df_transpose$'Augusta-Aiken'  )
#df_transpose$'Chattanooga'   <- as.numeric(df_transpose$'Chattanooga'  )
#df_transpose$'Columbus, GA (Opelika, AL)'  <- as.numeric(df_transpose$'Columbus, GA (Opelika, AL)'  )
#df_transpose$Macon  <- as.numeric(df_transpose$Macon )
#df_transpose$Savannah   <- as.numeric(df_transpose$Savannah  )
#df_transpose$'Tallahassee-Thomasville'  <- as.numeric(df_transpose$'Tallahassee-Thomasville' )
#df_transpose$Unknown   <- as.numeric(df_transpose$Unknown  )

#ok back to what needed to do below

#create summary row and column #have to switch this every time different number of DMAs
df_transpose$NorthCarolina = rowSums(df_transpose[,c(1,2,3,4,5,6)])

#remove the combined so it doesn't get added:

rows <- c("Combined")
combined <- df_transpose[rownames(df_transpose) %in% rows, ]
df_transpose <- df_transpose[!(row.names(df_transpose) %in% rows),]

#get total row 
df_transpose[nrow(df_transpose) + 1,] = colSums(df_transpose)
row.names(df_transpose)[6] <- "Total"

#add combined back in
df_transpose <- rbind(df_transpose,combined)

#save
write.csv(df_transpose,"frequency_count_NC.csv")



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
write.csv(df_transpose,"frequency_count_GA_December2022.csv")

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