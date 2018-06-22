###########################################
# Packages Management
###########################################
#rm(list = ls())

#library
list.of.packages <- c("lubridate","foreign", "readr", "RODBC","data.table", "ggplot2","dplyr", "reshape", "stringr","fst","sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

GetStat <- function(Side, Info) {
  Stat <- as.numeric(TeamStat.df %>% filter(team==Side) %>% select(Info))
  return(Stat)
}

GetAvgPoints <- function(Country, MatchType) {
  if (MatchType=='ALL') {
    Numberofmatch <- as.numeric(Historical.df %>% filter(home_team==Country | away_team==Country) %>% count())
    home_score <- as.numeric(Historical.df %>% filter(home_team==Country) %>% summarise(home_score=sum(home_score)))
    away_score <- as.numeric(Historical.df %>% filter(away_team==Country) %>% summarise(away_score=sum(away_score)))
  }
  else {
    Numberofmatch <- as.numeric(Historical.df %>% filter((home_team==Country | away_team==Country) & tournament==MatchType) %>% count())
    home_score <- as.numeric(Historical.df %>% filter(home_team==Country & tournament==MatchType) %>% summarise(home_score=sum(home_score)))
    away_score <- as.numeric(Historical.df %>% filter(away_team==Country & tournament==MatchType) %>% summarise(away_score=sum(away_score)))
  }
  Avgpoints <- (home_score+away_score)/Numberofmatch
}

Scoring <- function(Home, Away) {
  
  Home.Offence <- GetStat(Home, 'New_attack')
  Home.Defence <- GetStat(Home, 'New_defence')
  Home.Avgpoints <- GetStat(Home, 'total_worldcup_match_average_points')
  Away.Offence <- GetStat(Away, 'New_attack')
  Away.Defence <- GetStat(Away, 'New_defence')
  Away.Avgpoints <- GetStat(Away, 'total_worldcup_match_average_points')
  
  if (Home=='Iceland' | Home=='Panama') {
    Home.Avgpoints <- GetAvgPoints(Home,'ALL')
  }
  if (Away=='Iceland' | Away=='Panama') {
    Away.Avgpoints <- GetAvgPoints(Away,'ALL')
  }
  
  Home.Score<-Home.Offence*Away.Defence*Home.Avgpoints
  Away.Score<-Away.Offence*Home.Defence*Away.Avgpoints
  
  Home.Result <- round(sum(replicate(5, rpois(1, Home.Score)))/5)
  Away.Result <- round(sum(replicate(5, rpois(1, Away.Score)))/5)
  
  return(paste0(Home.Result,':', Away.Result))
}

Promotion <- function(Group) {
  x <- expand.grid(Group$GroupRank, Group$GroupRank) 
  x <- x %>% mutate(Rank1=left(as.character(Var1),1), Rank2=left(as.character(Var2),1), 
                    Gp1=right(as.character(Var1),1), Gp2=right(as.character(Var2),1)) %>%
    filter(Rank1!=Rank2 & Gp1!=Gp2 & Rank1<Rank2) %>%
    select(Var1, Var2) %>%
    arrange(Var1) 
  
  x$Var1 <- as.character(x$Var1)
  x$Var2 <- as.character(x$Var2)
  
  x <-inner_join(inner_join(x, GroupMatchSummary[,c('Home.Team.Name','GroupRank')], by=c('Var1'='GroupRank')), 
                 GroupMatchSummary[,c('Home.Team.Name','GroupRank')], by=c('Var2'='GroupRank')) %>%
    mutate(GroupRank=paste0(Var1,Var2)) %>%
    select(Home.Team.Name.x, Home.Team.Name.y, GroupRank) 
  names(x)[1:2] <- c('Home.Team.Name','Away.Team.Name')
  return(x)
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

MatchSchedule.df <- read.csv("https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/05%202018%20World%20Cup%20Match%20Fixtures.csv")
TeamStat.df <- read.csv("https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/01%202018%20World%20Cup%20Team%20Statistics.csv")
# HistoricalResult.df <- read.csv("https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/02%20Historical%20Data%20on%20World%20Cup%20Matches.csv")
# PlayerStat18.df <- read.csv("https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/05%202018%20World%20Cup%20Match%20Fixtures.csv")
url1000 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
Historical.df <- read.csv(url1000)

#SCRAPE LATEST MATCH RESULT
url999<-"https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Script.R"
source(url999)

df.schedule1$home_team <- ifelse(df.schedule1$home_team=='South Korea','Korea Republic',df.schedule1$home_team)
df.schedule1$away_team <- ifelse(df.schedule1$away_team=='South Korea','Korea Republic',df.schedule1$away_team)
df.result2$home_team <- ifelse(df.result2$home_team=='South Korea','Korea Republic',df.result2$home_team)
df.result2$away_team <- ifelse(df.result2$away_team=='South Korea','Korea Republic',df.result2$away_team)
v.cty[28] <- 'Korea Republic'
v.cty <- sort(v.cty)

TeamStat.df <- TeamStat.df %>% arrange(team)
TeamStat.df <- TeamStat.df %>% bind_cols(New_attack=sapply(v.cty,function(x) attack_rating2(x)))
TeamStat.df <- TeamStat.df %>% bind_cols(New_defence=sapply(v.cty,function(x) defence_rating2(x)))

Simulation_result <- data.frame("Champion"=integer(0), "First runner up"=integer(0), "Seond runner up"=integer(0), 
                                "Final.Home.Score"=integer(0), "Final.Away.Score"=integer(0),
                                "Third.Home.Score"=integer(0), "Third.Away.Score"=integer(0))

for (i in 1:10000) {
  MatchSchedule.df$Home.Team.Name <- as.character(MatchSchedule.df$Home.Team.Name)
  MatchSchedule.df$Away.Team.Name <- as.character(MatchSchedule.df$Away.Team.Name)
  
  #Prediction of Group Match Result
  GroupPrediction <- MatchSchedule.df %>% filter(Stage %in% c('1','2','3')) %>% select(Stage, Home.Team.Name, Away.Team.Name, Group)
  GroupPrediction <- cbind(GroupPrediction, as.data.frame(apply(GroupPrediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(GroupPrediction)[5] <- "PredictionResult" 
  GroupPrediction$PredictionResult <- as.character(GroupPrediction$PredictionResult)
  
  GroupPrediction <- GroupPrediction %>% mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)))

  GroupPrediction <- left_join(GroupPrediction,df.schedule1, by=c('Home.Team.Name'='home_team','Away.Team.Name'='away_team')) %>% 
                      mutate(Home.Score=ifelse(!is.na(home_score), home_score, Home.Score), 
                             Away.Score=ifelse(!is.na(away_score), away_score, Away.Score)) %>%
                      select(c(1:4,6:7)) %>%
                      mutate(Result=ifelse(Home.Score>Away.Score, 'Home', ifelse(Home.Score==Away.Score, 'Draw', 'Away')), 
                             Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, Away.Team.Name), 
                             Loser=ifelse(Home.Score>Away.Score, Away.Team.Name, Home.Team.Name))
  
  #Generate Summary table of all country
  CountryList <- GroupPrediction %>% select(Home.Team.Name, Group) %>% distinct(Home.Team.Name, Group)
  
  #Calculate score 
  WinScore <- GroupPrediction %>% filter(Result!='Draw') %>% group_by(Winner) %>% summarise(Win_score=n())
  Draw_Win_Score <- GroupPrediction %>% filter(Result=='Draw') %>% group_by(Winner) %>% summarise(Draw_score=n())
  Draw_Lose_Score <- GroupPrediction %>% filter(Result=='Draw') %>% group_by(Loser) %>% summarise(Draw_score=n())
  Home.Summary <- GroupPrediction %>% group_by(Home.Team.Name) %>% summarise(Home.Gain=sum(Home.Score), Home.Loss=sum(Away.Score))
  Away.Summary <- GroupPrediction %>% group_by(Away.Team.Name) %>% summarise(Away.Gain=sum(Away.Score), Away.Loss=sum(Home.Score))
  
  GroupMatchSummary <- full_join(full_join(full_join(full_join(full_join(CountryList, WinScore, by=c('Home.Team.Name'='Winner')), Draw_Win_Score, 
                                  by=c('Home.Team.Name'='Winner')), Draw_Lose_Score, by=c('Home.Team.Name'='Loser')),
                                  Home.Summary, by=c('Home.Team.Name')), Away.Summary, by=c('Home.Team.Name'='Away.Team.Name')) %>%
                        mutate(Win_score=as.numeric(ifelse(is.na(Win_score), 0, Win_score)), 
                               Draw_score.x=as.numeric(ifelse(is.na(Draw_score.x), 0, Draw_score.x)),
                               Draw_score.y=as.numeric(ifelse(is.na(Draw_score.y), 0, Draw_score.y)),
                               TotalScore=(Win_score*3)+Draw_score.x+Draw_score.y,
                               TotalGain=Home.Gain+Away.Gain, 
                               TotalLoss=Home.Loss+Away.Loss, 
                               Difference=TotalGain-TotalLoss,
                               Group=as.character(Group)) %>%
                        group_by(Group) %>%
                        select(Home.Team.Name, Group, TotalScore, TotalGain, TotalLoss, Difference) %>%
                        arrange(Group, desc(TotalScore), desc(Difference), desc(TotalGain), Home.Team.Name) %>%
                        mutate(Rank=row_number(), 
                               GroupRank=paste0(Rank, right(Group,1)),
                               Gp=right(Group,1)) %>% 
                        filter(Rank=='1' | Rank=='2')
  
  #Clear useless data table
  remove('Away.Summary', 'Draw_Lose_Score', 'Draw_Win_Score', 'Home.Summary','WinScore')
  
  #Promotion to Round 16
  Group1 <- Promotion(GroupMatchSummary %>% filter(Gp=='A' | Gp=='B') )
  Group2 <- Promotion(GroupMatchSummary %>% filter(Gp=='C' | Gp=='D') )
  Group3 <- Promotion(GroupMatchSummary %>% filter(Gp=='E' | Gp=='F') )
  Group4 <- Promotion(GroupMatchSummary %>% filter(Gp=='G' | Gp=='H') )
  
  #Prediction of Round 16
  Round16Prediction <- rbind(Group1, Group2, Group3, Group4)
  
  #Clear useless data table
  remove('Group1','Group2','Group3','Group4')
  
  Round16Prediction <- cbind(Round16Prediction, as.data.frame(apply(Round16Prediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(Round16Prediction)[4] <- "PredictionResult" 
  Round16Prediction$PredictionResult <- as.character(Round16Prediction$PredictionResult)
  
  Round16Prediction <- Round16Prediction %>% 
                        bind_cols(random=(runif(8,0,1)>=0.5), PromotionGroup=c(1,3,1,3,2,4,2,4)) %>% 
                        mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)),
                                Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, 
                                              ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Home.Team.Name, Away.Team.Name), 
                                                     Away.Team.Name))) %>%
                        group_by(PromotionGroup) %>%
                        mutate(id=row_number())
  
  #Prediction of ROund8
  Round8Prediction <- dcast(Round16Prediction, PromotionGroup~id, value.var='Winner')
  names(Round8Prediction)[2:3] <- c('Home.Team.Name','Away.Team.Name')
  Round8Prediction <- cbind(Round8Prediction, as.data.frame(apply(Round8Prediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(Round8Prediction)[4] <- "PredictionResult" 
  Round8Prediction$PredictionResult <- as.character(Round8Prediction$PredictionResult)
  
  Round8Prediction <- Round8Prediction %>% 
                        bind_cols(random=(runif(4,0,1)>=0.5), PromotionGroup1=c(1,1,2,2)) %>%
                        mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)),
                               Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, 
                                             ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Home.Team.Name, Away.Team.Name), 
                                                    Away.Team.Name))) %>%
                        group_by(PromotionGroup1) %>%
                        mutate(id=row_number())
  
  Round4Prediction <- dcast(Round8Prediction, PromotionGroup1~id, value.var='Winner')
  names(Round4Prediction)[1:3] <- c('PromotionGroup','Home.Team.Name','Away.Team.Name')
  Round4Prediction <- cbind(Round4Prediction, as.data.frame(apply(Round4Prediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(Round4Prediction)[4] <- "PredictionResult" 
  Round4Prediction$PredictionResult <- as.character(Round4Prediction$PredictionResult)
  
  Round4Prediction <- Round4Prediction %>% 
                        bind_cols(random=(runif(2,0,1)>=0.5), PromotionGroup1=c(1,1)) %>%
                        mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)),
                               Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, 
                                             ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Home.Team.Name, Away.Team.Name), 
                                                    Away.Team.Name)),
                               Loser=ifelse(Home.Score<Away.Score, Home.Team.Name, 
                                             ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Away.Team.Name, Home.Team.Name), 
                                                    Away.Team.Name))) %>%
                        group_by(PromotionGroup1) %>%
                        mutate(id=row_number())
  
  FinalPrediction <- dcast(Round4Prediction, PromotionGroup1~id, value.var='Winner')
  names(FinalPrediction)[1:3] <- c('PromotionGroup','Home.Team.Name','Away.Team.Name')
  FinalPrediction <- cbind(FinalPrediction, as.data.frame(apply(FinalPrediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(FinalPrediction)[4] <- "PredictionResult" 
  FinalPrediction$PredictionResult <- as.character(FinalPrediction$PredictionResult)
  
  FinalPrediction <- FinalPrediction %>%
                      bind_cols(random=(runif(1,0,1)>=0.5)) %>%
                      mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)),
                             Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, 
                                           ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Home.Team.Name, Away.Team.Name), 
                                                  Away.Team.Name)),
                             Loser=ifelse(Home.Score<Away.Score, Home.Team.Name, 
                                          ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Away.Team.Name, Home.Team.Name), 
                                                 Away.Team.Name)))
  
  ThirdPrediction <- dcast(Round4Prediction,PromotionGroup1~id, value.var='Loser')
  names(ThirdPrediction)[1:3] <- c('PromotionGroup','Home.Team.Name','Away.Team.Name')
  ThirdPrediction <- cbind(ThirdPrediction, as.data.frame(apply(ThirdPrediction[,c('Home.Team.Name', 'Away.Team.Name')], 1, function(x) Scoring(x[1],x[2]))))
  names(ThirdPrediction)[4] <- "PredictionResult" 
  ThirdPrediction$PredictionResult <- as.character(ThirdPrediction$PredictionResult)
  
  ThirdPrediction <- ThirdPrediction %>%
                      bind_cols(random=(runif(1,0,1)>=0.5)) %>%
                      mutate(Home.Score=as.numeric(left(PredictionResult, 1)), Away.Score=as.numeric(right(PredictionResult,1)),
                      Winner=ifelse(Home.Score>Away.Score, Home.Team.Name, 
                                     ifelse(Home.Score==Away.Score,ifelse(random==TRUE, Home.Team.Name, Away.Team.Name), 
                                            Away.Team.Name)))
  
  Simulation_Result <- c(FinalPrediction$Winner, FinalPrediction$Loser, ThirdPrediction$Winner, 
    FinalPrediction$Home.Score, FinalPrediction$Away.Score, ThirdPrediction$Home.Score, ThirdPrediction$Away.Score)
  
  Simulation_result[nrow(Simulation_result)+1,] <- Simulation_Result

  remove('CountryList','FinalPrediction','GroupMatchSummary','GroupPrediction','Round16Prediction','Round8Prediction','Round4Prediction',
         'ThirdPrediction')
}


df.summary <- Simulation_result %>% 
                group_by(Champion) %>%
                summarise(freq=n()) %>%
                arrange(desc(freq))