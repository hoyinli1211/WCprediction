library(rvest)

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

  #teams statistics
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

  #team
df.team.topgoals <- read_html(url.team.topgoals) %>%
                html_nodes("table") %>%
                .[1] %>%
                html_table(fill=TRUE) %>%
                .[[1]]

df.team.attempts <- read_html(url.team.attempts) %>%
                      html_nodes("table") %>%
                      .[1] %>%
                      html_table(fill=TRUE) %>%
                      .[[1]]

df.team.disciplinary <- read_html(url.team.disciplinary) %>%
                          html_nodes("table") %>%
                          .[1] %>%
                          html_table(fill=TRUE) %>%
                          .[[1]]

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
