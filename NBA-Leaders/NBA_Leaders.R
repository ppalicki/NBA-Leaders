#recreate the NY Times baseball all time stats for the NBA- using total rebounds as test
library("dplyr")
library("rvest")
library(tidyr)
library(ggplot2)
url <- ("http://www.basketball-reference.com/leaders/trb_season.html")
url2 <- html(url)
trbdata2 <- html_nodes(url2, "td:nth-child(2) .stats_table") %>% 
  html_table() 
trbdata2 <- trbdata2[[1]]
trbdata2 <- trbdata2 %>% 
  mutate(HOF = ifelse(grepl("\\*",trbdata2$Player), "HOF" , "Not HOF"))
#get rid of asterisk in name
trbdata2 <- trbdata2 %>%  mutate(Player= gsub("\\*","",trbdata2$Player))

#all time leaders
#identify the values that broke the single-season record
trb.best <- trbdata2 %>% 
  group_by(Season) %>% 
  summarise(Leader= max(TRB, na.rm=T))
trb.best <- mutate(trb.best, Hist.Leader=cummax(Leader))
season.best <- filter(trb.best, Leader==Hist.Leader)
season.best <- mutate(season.best, Metric="TRB")
#tidy up the main df
trbdata2 <- gather(trbdata2,"Metric","Value", TRB)
trbdata3 <- inner_join(trbdata2,trb.best, by="Season")
#ggplot of rebounding leaders
trb_plot <- ggplot(trbdata2, aes(Season,Value)) +
  geom_point()+geom_smooth(color="red")+
  geom_point(data=season.best, aes(Season, Hist.Leader),color="red", size=3)
trb_plot
write.table(trbdata2,file="trbdata.csv",sep=",",row.names=F)
write.table(trb.best,file="trbbest.csv",sep=",",row.names=F)
