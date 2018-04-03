

rm(list = ls())

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

install.packages("ggpubr")

library(ggpubr)

library(ggplot2)

library(scales)

library(stargazer)

library(dplyr)

library(psych)

library(grid)

library(gridExtra)


#### Descriptive Stats for Treatment and Control ####

setwd("/Users/stellacarneiro/Desktop")

matched_df <- read.csv("/Users/stellacarneiro/Desktop/Matched.Sample.csv",header = TRUE,sep = ",")

df.Total <- read.csv("/Users/stellacarneiro/Desktop/Data.Long.Final.csv",header = TRUE,sep = ",")

df.Treated <- matched_df[matched_df$Treated == 1, ] 

df.Control <- matched_df[matched_df$Treated == 0, ] 

##### Pie Charts to compare #####

tot <- as.data.frame(table(df.Total$Size.by.Assets.and.Sector))

matched <- as.data.frame(table(matched_df$Size.by.Assets.and.Sector))

matched$Perc <- matched$Freq / sum(matched$Freq) * 100

tot$Perc <- tot$Freq / sum(tot$Freq) * 100

tot <- merge(tot[ ,c(1,3)],matched[ ,c(1,3)],by=c("Var1"))


colnames(tot) <- c("Size","Perc","PercMarched")



blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


pie.tot <- ggplot(tot, aes(y = Perc, x ="", fill = Size)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y",start = 0) +
      geom_text(aes(y = Perc, label = paste0(round(tot$Perc,0),"%")), size=4, position = position_stack(vjust = 0.5)) +
      theme_minimal() + 
      blank_theme +
      theme(axis.text.x=element_blank()) +
      labs(fill = "Size") + 
      scale_fill_brewer("", palette = "YlOrRd") 
 
pie.matched <- ggplot(tot, aes(y = PercMarched, x ="", fill = Size)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y",start = 0) +
      geom_text(aes(y = PercMarched, label = paste0(round(tot$PercMarched,0),"%")), size=4, position = position_stack(vjust = 0.5)) +
      theme_minimal() + 
      blank_theme +
      theme(axis.text.x=element_blank()) +
      labs(fill = "Size") + 
      scale_fill_brewer("", palette = "YlOrRd") 

g <- ggarrange(pie.tot, pie.matched, labels = c("Total", "Exporters"),
          font.label = list(size = 12, color = "black", face = "plain", family = NULL),
          common.legend = TRUE, legend = "bottom", widths = c(1, 1))
g

ggsave(file="/Users/stellacarneiro/Google Drive/Dissertação - Parte 5/Images/Size_Total_Exporters_Assets.pdf", g, width = 4,height = 2.5) 

