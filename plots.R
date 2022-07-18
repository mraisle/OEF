#ok, now we're going to make some plots in our clean new script 
library(ggplot2)
library(dplyr)
library(here)
library(stringr)
library(tidyr)


#make my theme
theme_meg <- function () { 
  theme_bw(base_family="Georgia") %+replace% 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
          axis.line = element_line(colour = "black"), axis.title.x=element_text(face = "bold", size = 12), 
          axis.title.y=element_text(face = "bold", size = 10, angle=90), plot.title=element_text(face = "bold", size = 14, hjust =.5), 
          axis.text.x=element_text(, size = 10, angle=45), axis.text.y=element_text(face = "bold", size = 8))
}
#call in the relevant files 

stationsum <- read.csv(here("stationsummary_GA.csv"))
seeCC <- read.csv(here("frequency_count_GA.csv"))

stationsum <- stationsum[order(-stationsum$Freq),]

#remove national stations
natlnetworks <-  paste(c("GOOGLE","SOCIAL MEDIA","CBS","CNN","MSNBC","NBC","ABC", "NYT", "RADIO", "NEWSPAPER", "BBC", "OAN", "WAPO"), collapse = '|')
#remove FOX row by hand as to not remove local fox station
localonly <-  stationsum %>% filter(!grepl(natlnetworks, Var1)) 
localonly <- localonly[-c(1), ]


#get natl counts too
natlnetworks <-  paste(c("GOOGLE","SOCIAL MEDIA","CBS","CNN","MSNBC","NBC","ABC","FOX", "NYT", "CBN", "OAN", "BBC", "USA", "WAPO"), collapse = '|')
#remove FOX row by hand as to not remove local fox station
nationalonly <-  stationsum %>% filter(grepl(natlnetworks, Var1)) 
nationalonly <- nationalonly[-c(2), ]
sum(nationalonly$Freq)

top10 <- head(localonly, 11)

top10$DMA <- c("Atlanta","Atlanta","Macon","Columbus", "Savannah", "Albany","Augusta", "Atlanta","Atlanta", "Atlanta","Chattanooga")

stationsummary <- ggplot(top10, mapping=aes(reorder(Var1,(-Freq)), y=Freq, fill=DMA))+
  geom_bar(stat="identity")+
  geom_text(aes(label=Freq, family="Georgia"), vjust=0)+
  labs(y="Number of Respondents\n", x="Station Name", title="Top 10 Cited Favorite Local News in GA\n",
       subtitle ="1Earth Pollfish survey May 2022")+
  theme_meg()
stationsummary

ggsave("stationsummary.png", stationsummary, path=here(), scale = 2, width=8, height=5, units=c("cm"))



