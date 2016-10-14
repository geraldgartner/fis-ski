#Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("worldcup_standings_mf.csv")

data_wc <- subset(data, wc_all_rank=="1")

data_wc_m_seqd <- data_wc_m[,seq(2, nrow(data_wc_m), 2)]

dummy <- 1
#All Leader in one df
data_all_leaders <- subset(data, wc_all_rank=="1" | downhill_rank =="1" | sl_ce_rank=="1" | giantslalom_rank=="1")
data_all_leaders <- select(data_all_leaders, gender, name, nation, wc_all_rank, downhill_rank, sl_ce_rank, giantslalom_rank)

wc_all_m <- ggplot(data_wc_m, aes(x = dummy, y = year,fill=nation)) +
  geom_tile(stat="identity", color="white")+
  facet_wrap(~nation)

print(wc_all_m)

alltitles <- ggplot(data_wc, aes(x=year, y=nation, fill=nation))+
  geom_bar(stat="identity", color="white", width=0.6)+
  facet_wrap(~gender, nrow=1)
print(alltitles)