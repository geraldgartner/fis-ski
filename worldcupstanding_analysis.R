#Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(data.table)

#style
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

data <- read.csv("worldcup_standings_mf.csv")

#data_wc <- subset(data, wc_all_rank=="1")
#data_wc_m_seqd <- data_wc_m[,seq(2, nrow(data_wc_m), 2)]

nationen <- c("FRA" = "grey",
             "SUI"= "grey",
             "ITA" = "grey",
             "SWE"=  "grey",
             "LIE" = "grey",
             "USA" = "grey",
             "CAN" = "grey",
             "LUX" = "grey",
             "SLO"  = "grey",
             "NOR" = "grey",
             "GER" = "grey",
             "CRO" = "grey",
             "FIN" = "grey",
             "SOV" = "grey", 
             "AUT" = "red")

dummy <- 1
#All Leader in one df
data_all_leaders <- subset(data, wc_all_rank=="1" | downhill_rank =="1" | sl_ce_rank=="1" | giantslalom_rank=="1")
data_all_leaders <- select(data_all_leaders, year, gender, name, nation, wc_all_rank, downhill_rank, sl_ce_rank, giantslalom_rank)

#Gather for chart
data_all_leaders <- data_all_leaders %>%
  gather(disziplin, rang, wc_all_rank:giantslalom_rank)
data_all_leaders <- subset(data_all_leaders, rang=="1")

#Annotation based on count
counttitles <- as.data.frame(table(data_all_leaders$name))
counttitles <- as.integer(counttitles$Var1)
counttitles <- as.vector(counttitles$freq)
data_all_leaders_merged <- inner_join(data_all_leaders, counttitles, by = c("name" = "Var1"))


wc_all <- ggplot(data_all_leaders_merged, aes(x = year, y = dummy,fill=nation)) +
  geom_tile(stat="identity", color="white", height=0.1)+
  facet_wrap(~disziplin + gender)+
  scale_fill_manual(values = nationen)+
  #geom_text_repel(data=subset(data_all_leaders_merged, nation=="AUT" & Freq>=10),
                  #aes(x=year,y=dummy,label=name))+
  theme_fivethirtyeight()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  

print(wc_all)

ggsave("wc_all.pdf", width=50, height=25, units = "cm")

#Top 3 Platzierungen facet für Gesamtweltcup
wctop3 <- subset(data, wc_all_rank=="1" | wc_all_rank =="2" | wc_all_rank =="3" )
wctop3 <- select(wctop3, year, name, gender, wc_all_rank, nation )

top3 <- ggplot(wctop3, aes(x = year, y = as.factor(wc_all_rank),fill=nation)) +
  geom_tile(stat="identity", color="white", height=0.1)+
  facet_wrap(~ gender)+
  scale_fill_manual(values = nationen)
  theme_fivethirtyeight()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
print(top3)
ggsave("top3.pdf", width=50, height=25, units="cm")

##############################
#attempt to make y-axis discrete failed
wctop3$rank <- as.factor(wctop3$wc_all_rank)
alltitles <- ggplot(wctop3, aes(x=year, y=rank, fill=nation))+
  geom_bar(stat="identity", color="white")+
  facet_wrap(~gender)+
  scale_fill_manual(values = nationen)+
  scale_y_discrete(labels=c("1"="1. Platz", "2"="2. Platz", "3"="3. Platz"))
print(alltitles)

+ scale_x_discrete("Cut", labels = c("Fair" = "F","Good" = "G",
                                     "Very Good" = "VG","Perfect" = "P","Ideal" = "I"))

write.csv(wctop3, file = "gesamtweltcup.csv", row.names = TRUE)
####################################
#Prepare dataframes for export

abfahrttop3 <- subset(data, downhill_rank =="1" | downhill_rank =="2" | downhill_rank =="3")
abfahrttop3 <- select(abfahrttop3, year, name, nation, gender, downhill_rank)
setnames(abfahrttop3, "downhill_rank","rank")
write.csv(abfahrttop3, file="abfahrtsweltcup.csv")

rtltop3 <- - subset(data, giantslalom_rank =="1" | giantslalom_rank =="2" | giantslalom_rank =="3")
rtltop3 <- select(rtltop3, year, name, nation, gender, giantslalom_rank)
setnames(rtltop3, "giantslalom_rank","rank")
write.csv(rtltop3, file="abfahrtsweltcup.csv")

