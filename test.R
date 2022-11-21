library(ggplot2)
library(dplyr)
library(here)
library(stringr)
library(tidyr)
library(formattable)
library(viridisLite)


df <- data.frame ("Type"  = c("IRA Sec. 50161", "One Modern Aluminum Plant", "Estimated US steel & ironmaking investments"),
                  "Amount" = currency(c(5.8,6,51))
                
)

df$Amount <- currency(df$Amount, digits = 1L)
theme_meg <- function () { 
  theme_bw(base_family="Georgia") %+replace% 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.line = element_line(colour = "black"), axis.title.x=element_text(face = "bold", size = 12), 
          axis.title.y=element_text(face = "bold", size = 10, angle=90), plot.title=element_text(face = "bold", size = 14, hjust =.5),
          legend.position="none",
          axis.text.x=element_text(, size = 10, angle=0), axis.text.y=element_blank())
}


money <- ggplot(df, mapping=aes(y=Amount, x=reorder(Type, Amount), fill=Type))+
  geom_bar(stat="identity")+
  ylim(0,55)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  geom_text(aes(label=Amount, family="Georgia"), vjust=-.5)+
  labs(y="USD (Billions)", x="", title="")+
  scale_fill_manual(values = c("#FA9C1b", "#3388FF", "#FFE800"))+
  
  theme_meg()
money

ggsave("money.png", money, path=here(), scale = 2, width=8, height=5, bg='transparent', units=c("cm"))



