
rm(list = ls())

library(ggplot2)

require(scales)

library(ggthemes)

library(data.table)

library(readxl)

setwd("/Users/stellacarneiro/Google Drive/Database")

df <- read.csv("Matched.Sample.csv",header = TRUE,sep = ",")

cn <- read_excel("~/Google Drive/Dissertação - Parte 5/Data from other sources/ExpYoY.xlsx")


################### BIG SECTOR #####################################


CNAE.Agro <-  c("Agriculture, Forestry and Fishing")

CNAE.Ind <- c("Mining and Quarrying",
              "Food, Beverage and Tobacco Product Manufacturing",
              "Manufacturing Industries",
              "Heavy Industries",
              "Machinery and Equipment Industries",
              "Construction",
              "Other Industries",
              "Wood & Paper Products Manufacturing",
              "Utilities")


CNAE.Services <- c("Information and Communication",
                   "Administrative and Support Service Activities",
                   "Human Health and Social Work Activities",
                   "Wholesale and Retail Trade",
                   "Accommodation and Food Service Activities",
                   "Transportation and Storage",
                   "Education and Recreation")


overall.sector <- NULL


for (i in 1:length(df$Sector)){
  
  if (is.element(df$Sector[i],CNAE.Agro)==T) {
    
    overall.sector[i] <- paste("Agro.")
    
  } else if (is.element(df$Sector[i],CNAE.Services)==T) {
    
    overall.sector[i] <- "Services"
    
  } else {overall.sector[i] <- "Ind."}
  
}


df["Overall.Sector"] = overall.sector


###########################################

dft <- data.table(df)

df <- dft[, list(Freq =.N), by=list(Overall.Sector,Sector)] 

df$Freq <- round((df$Freq/sum(df$Freq))*100,digits = 1)

df <- df[order(df$Overall.Sector,df$Sector),]


dfs <- df[(df$Overall.Sector == "Services" & df$Freq < 3),]

s.new <- list("Services","Other Services",sum(dfs$Freq))

df <- df[!(df$Overall.Sector == "Services" & df$Freq < 3), ]

df <- rbind(df,s.new)

dfi <- df[(df$Overall.Sector == "Ind." & df$Freq < 1),]

i.new <- list("Ind.","Other Industries",sum(dfs$Freq))

df <- df[!(df$Overall.Sector == "Ind." & df$Freq < 1), ]

df <- rbind(df,i.new)


########################################

#### SECTORS TO ENGLISH #####

# GDP (Expenditures Approach)

cn["Exp"] <- round(((cn$`2007`)/sum(cn$`2007`))*100,digits = 1)


##### DONUTS CHARTS #####

#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(0.7*1.08, 1*1.08)) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels,cex=0.8)
  

  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA,cex=0.8)
  
  
}


df[ ,3] <- lapply(df[ ,3], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

df$Overall.Sector <- as.factor(df$Overall.Sector)

dfp <- aggregate(Freq ~ Overall.Sector, df, sum)

cnp <- aggregate(Exp ~ `Overall Sector`, cn, sum)


with(df,
     donuts(Freq, Overall.Sector, sprintf('%s: %s%%', Sector, Freq),
     col = c('seagreen3','steelblue2','tomato2'))
)

text(x = c(0.7, -0.05, 0.08), y = c(0.05, 0.3, -0.35), 
     labels = sprintf('%s: %s%%', dfp$Overall.Sector, dfp$Freq), col = 'black', cex = 0.9)


with(cn,
     donuts(Exp, `Overall Sector`, sprintf('%s: %s%%', Sectors, Exp),
            col = c('seagreen3','steelblue2','tomato2'))
)

text(x = c(0.5, -0.42, 0.55), y = c(0.08, 0, -0.15), 
     labels = sprintf('%s: %s%%', cnp$`Overall Sector`, round(cnp$Exp,digits = 1)), col = 'black', cex = 0.9)




