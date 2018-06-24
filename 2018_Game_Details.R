library(rvest)
library(ggplot2)

rm(list=ls())
#######################
#function
#######################
name2 <- function(name){
  
  v.name <- strsplit(name," ")[[1]]
  v.first <- 
  v.last <- as.character()
  
  for (i in v.name) {
    if (toupper(i)==i) {
      v.last <- paste(trimws(v.last),i,sep=" ")  
    }
    else {
      v.first <- paste(trimws(v.first),i,sep=" ")        
    }
  }
  v.name.new <- trimws(paste(trimws(v.last),trimws(v.first),sep=" "))
  return(v.name.new)
}

# v.player <- df.player.scored$player
# v.name.new <- sapply(v.player, function(x) nameTransformation(x))
# v.name.new
#######################
#url 
#######################
  #match result
url.match.result <- "https://projects.fivethirtyeight.com/soccer-api/international/2018/wc_matches.csv"

  #teams statistics
url.team.main <- "https://projects.fivethirtyeight.com/soccer-api/international/2018/wc_forecasts.csv"
url.team.rank <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/01%202018%20World%20Cup%20Team%20Statistics.csv"
url.team.topgoals <- "https://www.fifa.com/worldcup/statistics/teams/goal-scored"
url.team.attempts <- "https://www.fifa.com/worldcup/statistics/teams/shots"
url.team.disciplinary <- "https://www.fifa.com/worldcup/statistics/teams/disciplinary"
  #players statistics
url.player.main <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/PlayerList.csv"
url.player.main2 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Player.csv"
url.player.profile <- "https://www.fifa.com/worldcup/players/browser/#player-by-position"
url.player.scored <- "https://www.fifa.com/worldcup/statistics/players/goal-scored"
url.player.saves <- "https://www.fifa.com/worldcup/statistics/players/saves"
url.player.shots <- "https://www.fifa.com/worldcup/statistics/players/shots"
url.player.disciplinary <- "https://www.fifa.com/worldcup/statistics/players/disciplinary"

#######################
#data scrape
#######################
  #match result
df.match.result <- read.csv(url.match.result) %>%
                    mutate_if(is.factor, as.character) %>%
                    select(c(1,4,5,13,14)) %>%
                    mutate(team1=ifelse(team1=='Iran','IR Iran',ifelse(team1=='South Korea','Korea Republic',team1)),
                           team2=ifelse(team2=='Iran','IR Iran',ifelse(team2=='South Korea','Korea Republic',team2)))
  #team
df.team.main <- read.csv(url.team.main) %>%
                  mutate_if(is.factor, as.character) %>%
                  group_by(team) %>%
                  summarise(group=head(group,1)) %>%
                  arrange(group) %>%
                  mutate(team=ifelse(team=='Iran','IR Iran',ifelse(team=='South Korea','Korea Republic',team)))
#View(df.team.topgoals) 

df.team.rank <- read.csv(url.team.rank) %>%
                  mutate_if(is.factor, as.character) %>%
                  mutate(team=ifelse(team=='Iran','IR Iran',ifelse(team=='South Korea','Korea Republic',team))) %>%
                  filter(team %in% df.team.main$team)

df.team.topgoals <- read_html(url.team.topgoals) %>%
                      html_nodes("table") %>%
                      .[1] %>%
                      html_table(fill=TRUE) %>%
                      .[[1]] %>%
                      mutate(team=trimws(substr(Team,1,nchar(Team)-4)),
                             team.code=substr(Team,nchar(Team)-2,nchar(Team))) 

df.team.attempts <- read_html(url.team.attempts) %>%
                      html_nodes("table") %>%
                      .[1] %>%
                      html_table(fill=TRUE) %>%
                      .[[1]] %>%
                      mutate(team=trimws(substr(Team,1,nchar(Team)-4)),
                             team.code=substr(Team,nchar(Team)-2,nchar(Team))) 

df.team.disciplinary <- read_html(url.team.disciplinary) %>%
                          html_nodes("table") %>%
                          .[1] %>%
                          html_table(fill=TRUE) %>%
                          .[[1]] %>%
                          mutate(team=trimws(substr(Team,1,nchar(Team)-4)),
                                 team.code=substr(Team,nchar(Team)-2,nchar(Team)))

df.team.main <- df.team.main %>% 
                  left_join(df.team.topgoals, by=c('team')) %>%
                  left_join(df.team.attempts, by=c('team')) %>%
                  left_join(df.team.disciplinary, by=c('team')) %>%
                  select(c(1,2,6:12,17:21,25:32))
colnames(df.team.main) <- c('team','group',
                            'goal.for','goal.scored','goal.against','goal.penalty','goal.ownfor','goal.openplay','goal.setpiece',
                            'shots','shots.ontarget','shots.offtarget','shots.blocked','shots.woodwork',
                            'match.played','match.yellow','match.yellow2red','match.red','match.foulcomitted','match.foulsuffered','match.penaltysuffered','team.code')
df.team.main <- df.team.main %>%
                  select(c(1,2,22,15,3:14,16:21))
#View(df.team.main)

###################
  # data exploratory on team statistics

    #rank difference versus score difference
df1 <- df.match.result %>% 
        left_join(df.team.rank[,c(1,9)], by=c('team1'='team')) %>%
        left_join(df.team.rank[,c(1,9)], by=c('team2'='team')) %>%
        mutate(score.diff=score1-score2,
               rank.diff=june_fifa_rank.x-june_fifa_rank.y,
               score.diff2=abs(score.diff),
               rank.diff2=ifelse(score.diff< 0, -rank.diff, rank.diff))

ggplot(df1%>%filter(!is.na(score1)), aes(x=rank.diff2, y=score.diff2)) + geom_point()


###################


  #player

df.player.main <- read.csv(url.player.main) 
df.player.main <- df.player.main %>%
                    mutate_if(is.factor, as.character)

df.player.main2 <- read.csv(url.player.main2) %>%
                    mutate_if(is.factor, as.character)

# df.player.profile <- read_html(url.player.profile) %>%
#                       html_nodes("table") %>%
#                       .[1] %>%
#                       html_table(fill=TRUE) %>%
#                       .[[1]] 

df.player.scored <- read_html(url.player.scored) %>%
                      html_nodes("table") %>%
                      .[1] %>%
                      html_table(fill=TRUE) %>%
                      .[[1]] 

df.player.saves <- read_html(url.player.saves) %>%
                    html_nodes("table") %>%
                    .[1] %>%
                    html_table(fill=TRUE) %>%
                    .[[1]]

df.player.shots <- read_html(url.player.shots) %>%
                    html_nodes("table") %>%
                    .[1] %>%
                    html_table(fill=TRUE) %>%
                    .[[1]]

df.player.disciplinary <- read_html(url.player.disciplinary) %>%
                            html_nodes("table") %>%
                            .[1] %>%
                            html_table(fill=TRUE) %>%
                            .[[1]]

df.player.FIFA <- df.player.scored %>%
                    full_join(df.player.saves, by=c('Player')) %>%
                    full_join(df.player.shots, by=c('Player')) %>%
                    full_join(df.player.disciplinary, by=c('Player'))

df.player.FIFA$Player2 <- sapply(df.player.FIFA$Player, function(x) name2(x))

df.player.FIFA <- df.player.FIFA %>%
                    mutate_if(sapply(df.player.FIFA, is.factor), as.character)

df.player.MAIN <- df.player.main %>% left_join(df.player.FIFA, by=c('FIFA.Popular.Name'='Player2'))

#checking
dim(df.player.MAIN %>% filter(!is.na(Player)))
unique(c(df.player.scored$Player,df.player.saves$Player, df.player.shots$Player, df.player.disciplinary$Player))

df.player.MAIN[!df.player.MAIN$FIFA.Popular.Name %in% df.player.FIFA$Player]$FIFA.Popular.Name
df.player.FIFA %>% filter(!df.player.FIFA$Player2 %in% df.player.MAIN$FIFA.Popular.Name) %>% select(Player2)
