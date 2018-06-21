###########################################
# Packages Management
###########################################
#rm(list = ls())

#library
list.of.packages <- c("lubridate","foreign", "readr", "RODBC","data.table", "ggplot2","dplyr", "reshape", "xlsx", "stringr","fst","sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

Scoring <- function(Home, Away) {
  
  Home.Offence <- (TeamStat.df %>% filter(team==Home))$Offence_rating
  Home.Defence <- (TeamStat.df %>% filter(team==Home))$Defence_rating
  Home.Avgpoints <- (TeamStat.df %>% filter(team==Home))$total_worldcup_match_average_points
  Away.Offence <- (TeamStat.df %>% filter(team==Away))$Offence_rating
  Away.Defence <- (TeamStat.df %>% filter(team==Away))$Defence_rating
  Away.Avgpoints <- (TeamStat.df %>% filter(team==Away))$total_worldcup_match_average_points
  
  Home.Score<-Home.Offence*Away.Defence*Home.Avgpoints
  Away.Score<-Away.Offence*Home.Defence*Away.Avgpoints
  
  Home.Result <- rpois(1, Home.Score)
  Away.Result <- rpois(1, Away.Score)

  return(paste0(Home.Result,':', Away.Result))
  
}

###################Data import##########################
MatchSchedule.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/05 2018 World Cup Match Fixtures.csv")
TeamStat.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/01 2018 World Cup Team Statistics.csv")
HistoricalResult.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/02 Historical Data on World Cup Matches.csv")
# PlayerStat17.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/04 2017 FIFA Players Database.csv")
PlayerStat18.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/03 2018 FIFA Players Database.csv")
QualResult.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/QualResult.csv")
PlayerList.df <- read.csv("C:/Users/ivanwong/OneDrive - DBS Bank Ltd/R_Script/FIFA 2018/Data/PlayerList.csv")

# WCCOUNTRYLIST <- (PlayerList.df %>% select(Team) %>% distinct(Team))$Team
# PlayerStat.df <- PlayerStat18.df %>% filter(Nationality %in% WCCOUNTRYLIST)
# SimTeamStat.df <- TeamStat.df %>% select(team,june_fifa_points)
# SimTeamStat.df$team <- as.character(SimTeamStat.df$team)
MatchSchedule.df$Home.Team.Name <- as.character(MatchSchedule.df$Home.Team.Name)
MatchSchedule.df$Away.Team.Name <- as.character(MatchSchedule.df$Away.Team.Name)

GroupPrediction <- MatchSchedule.df %>% filter(Stage %in% c('1','2','3'))

apply(GroupPrediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))

url3 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
df.result <- read.csv(url3)

