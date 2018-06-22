################################
# DBS World Cup competition
#2018-06-14
################################
#reference: 
#https://fivethirtyeight.com/features/how-our-2018-world-cup-predictions-work/
#https://www.mango-solutions.com/blog/another-prediction-of-fifa-world-cup-2018
#https://rviews.rstudio.com/2018/06/14/player-data-for-the-2018-fifa-world-cup/ 
#http://www.collective-behavior.com/publ/ELO.pdf

################################
#Package management
################################
list.of.packages <- c("tidyverse","stringr", "rvest","dplyr","ggplot2","cowplot","tabulizer","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

################################
#Data preperation
################################

# reference
# https://fivethirtyeight.com/features/how-our-2018-world-cup-predictions-work/

#2018 Worldcup Russia schedule
url1 <- "https://projects.fivethirtyeight.com/soccer-api/international/2018/wc_matches.csv"
url2 <- "https://projects.fivethirtyeight.com/soccer-api/international/2018/wc_forecasts.csv"
df.schedule1 <- read.csv(file=url1) %>%
                  mutate(date=as.Date(as.character(date)),
                         home_team=as.character(team1),
                         home_team=ifelse(substr(home_team,1,5)=='Korea','South Korea',home_team),
                         away_team=as.character(team2),
                         away_team=ifelse(substr(away_team,1,5)=='Korea','South Korea',away_team),
                         home_score=score1,
                         away_score=score2,
                         tournament='FIFA World Cup',
                         city='',
                         country='Russia',
                         neutral=ifelse(team1!='Russia' & team2!='Russia',TRUE,FALSE),
                         weight=0.8)
df.schedule1 <- df.schedule1[,c(1,21:ncol(df.schedule1))]
df.schedule2 <- read.csv(file=url2) 
#View(df.schedule1)
#View(df.schedule2)

v.cty <- unique(as.character(df.schedule2$team))

#historical match result
url3 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
df.result <- read.csv(url3)
#View(df.result)
df.result2 <- df.result %>%
  mutate(date=as.Date(as.character(date)),
         home_team=as.character(home_team),
         away_team=as.character(away_team),
         tournament=as.character(tournament),
         city=as.character(city),
         country=as.character(country),
         weight = ifelse(tournament %in% c('FIFA World Cup','FIFA World Cup qualification'), 0.8, 
                         ifelse(tournament ==  'Friendly', 0.2,0.4))) %>% 
  filter(home_team %in% v.cty & away_team %in% v.cty) %>% # filter those attend 2018 world cup only
  filter((home_team == 'Russia' & neutral == FALSE) | (home_team != 'Russia' & away_team != 'Russia' & neutral == TRUE)) %>% # consider home effect
  filter(date >= as.Date('2000-01-01')) %>%
  bind_rows(df.schedule1) %>%
  filter(!is.na(home_score))#filter those long time ago


#players data
url4 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Player.csv"
df.player <- read.csv(url4)
#View(df.player)

#players data2
url5 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/PlayerList.csv"
df.player18 <- read.csv(url5)
#View(df.player18)

#team data
url6 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/01%202018%20World%20Cup%20Team%20Statistics.csv"
df.team <- read.csv(url6)
#View(df.team)
