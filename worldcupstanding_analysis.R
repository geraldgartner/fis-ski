#Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(data.table)
library(ggthemes)
library(gridExtra)
library(ggvis)

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

data$gender <- sub("M", "Herren", data$gender)
data$gender <- sub("L", "Damen", data$gender)

#data_wc <- subset(data, wc_all_rank=="1")
#data_wc_m_seqd <- data_wc_m[,seq(2, nrow(data_wc_m), 2)]

nationen <- c("FRA" = "grey",
              "AND" = "grey", 
              "ARG" = "grey", 
              "AUS" = "grey", 
              "BEL" = "grey", 
              "BLR" = "grey", 
              "BUL" = "grey", 
              "CZE" = "grey", 
              "DEN" = "grey", 
              "FIN" = "grey", 
              "GBR" = "grey",
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
             "AUT" = "red", 
             "RUS" = "grey", 
             "ESP" = "grey",
             "NZL" = "grey", 
             "Marcel Hirscher" = "red", 
             "Alle anderen Athleten" = "grey" )


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


abfahrttop3$rank <- as.factor(abfahrttop3$rank)
abfahrt <- ggplot(abfahrttop3, aes(x=year, y=rank, fill=nation, label=name))+
  geom_tile(stat="identity", color="white")+
  facet_wrap(~gender)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = nationen)+
  scale_y_discrete(limits=c("3","2","1"),
                   labels=c("3.Platz", "2. Platz", "1. Platz" ))+
  ggtitle("Platzierungen der ÖSV-Athleten in der Abfahrt")

print(abfahrt)
ggsave("abfahrt.pdf", width=50, height=25, units="cm")

abfahrttop4 <- abfahrttop3 %>%
  ggvis(x=~year, y=~rank, fill=~nation) %>%
  layer_rects(width=band(1), height=band(1)) %>%
  scale_fill_manual(values = nationen) %>%
  scale_y_discrete(limits=c("3","2","1"),
                   labels=c("3.Platz", "2. Platz", "1. Platz" ))
abfahrttop4

##############
rtltop <- select(data, year, name, nation, gender, giantslalom_rank)
as.numeric(rtltop$giantslalom_rank)
rtltop <- subset(rtltop, giantslalom_rank == "1" | giantslalom_rank =="2" | giantslalom_rank =="3")
setnames(rtltop, "giantslalom_rank","rank")
write.csv(rtltop, file="riesentorlauf.csv")

rtltop$rank <- as.factor(rtltop$rank)
rtltop <- ggplot(rtltop, aes(x=year, y=rank, fill=nation))+
  geom_tile(stat="identity", color="white")+
  facet_wrap(~gender)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = nationen)+
  scale_y_discrete(limits=c("3","2","1"),
                   labels=c("3.Platz", "2. Platz", "1. Platz" ))+
  ggtitle("Platzierungen der ÖSV-Athleten im Riesentorlauf")

print(rtltop)
ggsave("rtltop.pdf", width=50, height=25, units="cm")



#####################

slalomtop <- select(data, year, name, nation, gender, sl_ce_rank)
slalomtop <- subset(slalomtop, sl_ce_rank == "1" | sl_ce_rank =="2" | sl_ce_rank =="3")
setnames(slalomtop, "sl_ce_rank","rank")
write.csv(slalomtop, file="slalom.csv")

unique(slalomtop$nation)

slalomtop$rank <- as.factor(slalomtop$rank)

slalomtop <- ggplot(slalomtop, aes(x=year, y=rank, fill=nation))+
  geom_tile(stat="identity", color="white")+
  facet_wrap(~gender)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = nationen)+
  scale_y_discrete(limits=c("3","2","1"),
                   labels=c("3.Platz", "2. Platz", "1. Platz"))+
  ggtitle("Platzierungen der ÖSV-Athleten im Slalom")

print(slalomtop)
ggsave("slalomtop.pdf", width=50, height=25, units="cm")

##########################################################

#Wie sehr hängt AT von Marcel Hirscher in den technischen Disziplinen ab?
#Dafür müssen Punkte pro nation pro Jahr summiert werden

slalomAT <- select(data, year, name, gender, sl_ce_pts, nation )
slalomAT <- subset(slalomAT, name !="HIRSCHER Marcel")
slalomAT <- slalomAT[complete.cases(slalomAT$sl_ce_pts), ] 
slalomAT$sl_ce_pts <- as.numeric(as.character(slalomAT$sl_ce_pts))

slalomnationsum <- slalomAT %>% 
                          group_by(nation, year, gender) %>% 
                          summarise(sl_ce_pts = sum(sl_ce_pts)) %>%
                          subset(year >=2005 & nation =="AUT" & gender =="Herren")
slalomnationsum$id <- "Alle anderen Athleten" 
slalomnationsum <- select(slalomnationsum, year, sl_ce_pts, id)


## Slalom-Punkte Österreichs und seit 1995 auf einzelne Athleten heruntergebrochen

sl <- subset(data, nation=="AUT" & gender =="Herren" & year >= 2005)
sl <- select(sl, year, nation, name, sl_ce_pts)
sl <- sl[complete.cases(sl$sl_ce_pts), ] 

slp <- ggplot(sl, aes(x=)) +
  geom_bar(stat="identity", aes(x=year)) +
print(slp)

slp <- ggplot(sl, aes(x = factor(year), y = sl_ce_pts, fill=factor(name))) + 
  geom_bar(stat = "identity")
print(slp)

#Nur Marcel Hirscher vs. Gesamt

slmh <- subset(data, nation=="AUT" & gender =="Herren" & name =="HIRSCHER Marcel")
slmh <- select(slmh, year, nation, sl_ce_pts)
slmh$id <- "Marcel Hirscher"

totalsl<-merge(slalomnationsum,slmh, all.x=TRUE, all.y=TRUE)
totalsl$id <- as.factor(totalsl$id)
totalsl <- totalsl[order(totalsl$id, decreasing = TRUE), ]
                        
totalslp <- ggplot(totalsl, aes(x = factor(year), y = sl_ce_pts, fill=factor(id))) + 
  geom_bar(stat = "identity")+
  labs(fill="")+
  scale_fill_manual(values = nationen)+
  ggtitle("Marcel Hirschers Anteil an allen ÖSV-Slalom-Weltcup-Punkten")+
  theme
print(totalslp) 

ggsave("totalslp.pdf", width = 50, height = 25, units="cm")

#Für RTL Weltcup das Gleiche

rtlAT <- select(data, year, name, gender, giantslalom_pts, nation )
rtlAT <- subset(rtlAT, name !="HIRSCHER Marcel")
rtlAT <- rtlAT[complete.cases(rtlAT$giantslalom_pts), ] 
rtlAT$giantslalom_pts <- as.numeric(as.character(rtlAT$giantslalom_pts))

rtlnationsum <- rtlAT %>% 
  group_by(nation, year, gender) %>% 
  summarise(giantslalom_pts = sum(giantslalom_pts)) %>%
  subset(year >=2005 & nation =="AUT" & gender =="Herren")
rtlnationsum$id <- "Alle anderen Athleten" 
rtlnationsum <- select(rtlnationsum, year, giantslalom_pts, id)

rtlmh <- subset(data, nation=="AUT" & gender =="Herren" & name =="HIRSCHER Marcel")
rtlmh <- select(rtlmh, year, nation, giantslalom_pts)
rtlmh$id <- "Marcel Hirscher"

totalrtl<-merge(rtlnationsum,rtlmh, all.x=TRUE, all.y=TRUE)
totalrtl$id <- as.factor(totalrtl$id)
totalrtl <- totalrtl[order(totalrtl$id, decreasing = TRUE), ]

totalrtl <- ddply(totalrtl, .(id), summarise, ratio = totalrtl[id == "Marcel Hirscher"] / totalrtl[id == "Alle anderen Athleten"])


totalrtlp <- ggplot(totalrtl, aes(x = factor(year), y = giantslalom_pts, fill=factor(id))) + 
  geom_bar(stat = "identity")+
  labs(fill="")+
  scale_fill_manual(values = nationen)+
  ggtitle("Marcel Hirschers Anteil an allen ÖSV-RTL-Weltcup-Punkten")+
  theme
print(totalrtlp) 



multiplot(totalslp, totalrtlp, cols=1)
ggall <- grid.arrange(totalslp, totalrtlp, ncol=1)