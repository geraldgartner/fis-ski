#Load Libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(ggThemeAssist)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(ggiraph)

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
ergebnisse <- read.csv("~/Google Drive/dStd.at/fis-ski/ergebnisse.csv")
uebersicht <- read.csv("~/Google Drive/dStd.at/fis-ski/subpages.csv")

#Merge data
uebersicht$eventid <- sub("http://data.fis-ski.com/dynamic/event-details.html?event_id=", "", uebersicht)

&cal_suchsector=AL

data <- merge(ergebnisse, uebersicht, x.by="raceurl", y.by="url")


