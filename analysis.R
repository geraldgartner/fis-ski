#Load Libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(ggThemeAssist)
library(tidyr)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(stringr)

#All data is from https://data.fis-ski.com/

#Style definieren
theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))  

#Load data 
ergebnisse <- read.csv("ergebnisse.csv")
uebersicht <- read.csv("subpages.csv")

uebersicht$eventid <- gsub("http://data.fis-ski.com/dynamic/event-details.html?event_id=", "$", uebersicht)

#Merge data
ergebnisse$athletid <- str_extract_all(ergebnisse$athleturl,"\\(?[,0-9]+\\)?")
ergebnisse$rennid <- str_extract_all(ergebnisse$raceurl,"\\(?[,0-9]+\\)?")
uebersicht$rennid <- str_extract_all(uebersicht$url,"\\(?[,0-9]+\\)?")

#Top Rows for testing
ergebnisse_top <- head(ergebnisse, 10)
as.vector(ergebnisse_top$athletid)
unlist(ergebnisse_top$url)
unlist(ergebnisse_top$url, recursive = TRUE)

as.numeric(c(ergebnisse$rennid, ergebnisse$athletid, uebersicht$rennid)
unlist((ergebnisse_top$url))
as.numeric(ergebnisse_top$url)
ergebnisse_top <- as.matrix(ergebnisse_top)



a <- (1:120)
b <- a[seq(1, length(a),6)]




