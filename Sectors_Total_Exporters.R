
rm(list = ls())

library(ggplot2)

library(grid)

require(scales)

library(ggthemes)

library(gridExtra)

library(data.table)

library(readxl)

library(dplyr)

setwd("/Users/stellacarneiro/Desktop")

df <- read.csv("Matched.Sample.csv",header = TRUE,sep = ",")

df <- df[df$Treated == 1, ] 

df <- df[!duplicated(df$Company.ID), ]

cn <- read.csv("/Users/stellacarneiro/Desktop/Data.Long.Final.csv",header = TRUE,sep = ",")

cn <- cn[!duplicated(cn$Company.ID), ]


HighExporters <- nrow(cn[cn$Export.Rate > 0.5, ])

HighExporters.2009 <- nrow(cn[cn$Export.Rate > 0.5 & cn$Year == 2009, ])

Nontreated <- nrow(cn[cn$Export.Rate == 0, ])



###################### All #####################

cnt <- data.table(cn)

cn <- cnt[, list(Freq =.N), by=list(Overall.Sector,Sector)] 

cn$Freq <- round((cn$Freq/sum(cn$Freq))*100,digits = 1)

cn <- cn[order(cn$Overall.Sector,cn$Sector),]



dft <- data.table(df)

df <- dft[, list(Freq =.N), by=list(Overall.Sector,Sector)] 

df$Freq <- round((df$Freq/sum(df$Freq))*100,digits = 1)

df <- df[order(df$Overall.Sector,df$Sector),]


df <- merge(cn,df,by=c("Sector","Overall.Sector"))

colnames(df) <- c("Sector","Overall.Sector","Total","Matched")



########################################

df <- df[order(df$Matched,decreasing = T),]

df$Rank <- rank(df$Matched)



df$Sector <- gsub("Agriculture, Forestry and Fishing","Agri.", df$Sector)

df$Sector <- gsub("Food, Beverage and Tobacco Product Manufacturing","Food, Beverage & Tobacco", df$Sector)

df$Sector <- gsub("Machinery and Equipment Industries","Machinery & Equip.", df$Sector)

df$Sector <- gsub("Transportation and Storage","Transp. & Storage", df$Sector)

df$Sector <- gsub("Administrative and Support Service Activities","Adm. & Support", df$Sector)

df$Sector <- gsub("and","&", df$Sector)


theme_set(theme_bw()) 

g.mid<-ggplot(df,aes(x=1,y=reorder(Sector, Rank)))+geom_text(aes(label=Sector))+
 geom_segment(aes(x=0.94,xend=0.9415,yend=Sector))+
# geom_segment(aes(x=1.04,xend=1.045,yend=Sector))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA), 
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = df, aes(x = reorder(Sector, Rank), y = Total, fill =Total )) + 
  geom_bar(stat = "identity") + ggtitle("Total") + scale_fill_gradient2(low="yellow", mid="orange", high="red")  +
  theme(axis.title.x = element_blank(), axis.text.x=element_text(size=rel(1.25)),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm"),
        panel.background = element_blank()) +
        theme(legend.position="none") +
        scale_y_reverse(labels = comma) + coord_flip()  

g2 <- ggplot(data = df, aes(x = reorder(Sector, Rank), y = Matched,fill = Matched)) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("Exporters") + scale_fill_gradient2(low="yellow", mid="orange", high="red")  +
  theme(axis.title.x = element_blank(), axis.text.x=element_text(size=rel(1.25)),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm"),panel.background = element_blank()) +
        theme(legend.position="none") +
        coord_flip() 


gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(3/9,3/9,3/9))

g <- arrangeGrob(gg1,gg.mid,gg2,ncol=3,widths=c(3/9,3/9,3/9)) 

ggsave(file="/Users/stellacarneiro/Google Drive/Dissertação - Parte 5/Images/Sectors_Total_Exporters.pdf", g, width = 10, height = 5) 
