
rm(list = ls())

library(ggplot2)

library(grid)

require(scales)

library(ggthemes)

library(gridExtra)

library(data.table)

library(readxl)

library(dplyr)

library(xtable)

library(stargazer)

library(devtools)

install_github("ChandlerLutz/starpolish")

setwd("/Users/stellacarneiro/Desktop")

df_total <- read.csv("/Users/stellacarneiro/Desktop/Data.Long.Final.csv",header = TRUE,sep = ",")

df_total[8:55] <- lapply(df_total[8:55], function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})

sapply(df, class)

df_matched <- df_total[!(is.na(df_total$Treated)), ] 


x<- c(-1,1)

df_total <- df_total[df_total$Real.Sales.Growth >=x[1] & df_total$Real.Sales.Growth <=x[2], ]

df_total <- df_total[df_total$Real.Investment >=x[1] & df_total$Real.Investment <=x[2], ]

x<- c(-100,100)

df_total <- df_total[df_total$Real.CF.to.K.in.t.1 >=x[1] & df_total$Real.CF.to.K.in.t.1 <=x[2], ]

df_total <- df_total[df_total$Real.CF.Growth >=x[1] & df_total$Real.CF.Growth <=x[2], ]


#### Compare Treated and Control

dfe <- df_matched[df_matched$Treated ==1, ] 

df <- df_matched[df_matched$Treated == 0, ] 

df_unt <- subset(df_total,!df_total$Company.ID %in% dfe$Company.ID)  

### Select columns

sel_col <- c( "Year",
              "Real.Investment",
              "Real.CF.Growth",
              "Real.CF.to.K.in.t.1",
              "Real.Sales.Growth",
              "Leverage",
              "Colateral",
              "Liquidity")

sel_col_mod <- c( "Year",
              "Investment",
              "Cash-Flow Growth",
              "Cash-Flow / Fixed Assets",
              "Sales Growth",
              "Leverage",
              "Colateral",
              "Liquidity")

df.select <- df[ ,sel_col]

dfe.select <- dfe[ ,sel_col]

df_unt.select <- df_unt[ ,sel_col]

#### Untreated

sum_unt_med <- as.data.frame(df_unt.select %>%
                               group_by(Year) %>% 
                               summarise_each(funs(mean(., na.rm = TRUE))))

sum_unt_med <- t(sum_unt_med)

colnames(sum_unt_med) <- as.character(sum_unt_med[1, ])

sum_unt_med <- as.data.frame(sum_unt_med[c(-1),c(-7)])

sum_unt_med$Group <- c(rep(paste("Untreated"),nrow(sum_unt_med)))

sum_unt_med$Order <- seq(1,nrow(sum_unt_med),1)

#### Control

sum_all_med <- as.data.frame(df.select %>%
                         group_by(Year) %>% 
                         summarise_each(funs(mean(., na.rm = TRUE))))

sum_all_med <- t(sum_all_med)

colnames(sum_all_med) <- as.character(sum_all_med[1, ])

sum_all_med <- as.data.frame(sum_all_med[c(-1),c(-1)])

sum_all_med$Group <- c(rep(paste("Control"),nrow(sum_all_med)))

sum_all_med$Order <- seq(1,nrow(sum_all_med),1)

### Matched

sum_matched_med <- as.data.frame(dfe.select %>%
                               group_by(Year) %>% 
                               summarise_each(funs(mean(., na.rm = TRUE))))

sum_matched_med <- t(sum_matched_med)

colnames(sum_matched_med) <- as.character(sum_matched_med[1, ])

sum_matched_med <- as.data.frame(sum_matched_med[c(-1),c(-1)])

sum_matched_med$Group <- c(rep(paste("Treated"),nrow(sum_matched_med)))

sum_matched_med$Order <- seq(1,nrow(sum_matched_med),1)

tot <- rbind(sum_matched_med,sum_all_med,sum_unt_med)

tot <- tot[order(tot$Order), ]

tot$Variable <-  c("Investment", "", "",
                "Cash-Flow Growth","","",
                "Cash Flow / Fixed Assets", "","",
                "Sales Growth", "","",
                "Leverage", "","",
                "Colateral", "","",
                "Liquidity", "","")

tot <- tot[ ,c("Variable","Group","2008","2009","2010","2011","2012","2013")]

tot$Group <- paste0("\\textit{", tot$Group, "}")

print(xtable(tot), include.rownames=FALSE)
