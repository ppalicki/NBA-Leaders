#this code scrapes the basketball reference site for the all time single season leaders
#for various categories- the source data has the top 250 for each metric
library("dplyr")
library("rvest")
library(tidyr)
library(ggplot2)
#enter metric names
metric <- c("trb","pts","mp","fg","fga","fg3","fg3a","ft","fta","orb","drb","ast","stl",
            "blk","fg3_pct","ft_pct","per","ts_pct","ws","ws_per_48","bpm","usg_pct")
#autoscrape
base_url <- "http://www.basketball-reference.com/leaders/"
fetch_leaders <- function(metric) {
  url <- paste0(base_url,metric,"_season.html")
  url2 <- html(url)
   data <- html_nodes(url2, "td:nth-child(2) .stats_table") %>% 
    html_table() 
  #table 1 is the NBA all-time leaders table (excludes ABA)
   data <- data[[1]]
  data <- data %>% 
    mutate(HOF = ifelse(grepl("\\*",data$Player), "HOF" , "Not HOF"))
  #get rid of asterisk in name
  data <- data %>%  mutate(Player= gsub("\\*","",data$Player))
  #rename 3rd column
  names(data)[3] <- "Stat"
  #identify values that broke record
  best <- data %>% 
    group_by(Season) %>% 
    summarise(Leader= max(Stat, na.rm=T))
  best <- mutate(best, Hist.Leader=cummax(Leader))
 
  #add metric column
  data <- mutate(data, Metric=metric)
   
  data <- inner_join(data,best, by="Season")
  Sys.sleep(0.5)
  data
}

nba_leaders<- lapply(metric, failwith(NULL,fetch_leaders))
nba_leadersdf <- bind_rows(nba_leaders)

#write table
write.table(nba_leadersdf,"nba_leaders.csv",sep=",",row.names=F)
#get team abbrev list
tm_abbr <- nba_leadersdf %>% 
  select(Tm) %>% 
 distinct(Tm) %>% 
  arrange(Tm)
write.table(tm_abbr,"nba_teams.csv",sep=",",row.names=F)
