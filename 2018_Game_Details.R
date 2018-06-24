library(rvest)

#######################
#url 
#######################

  #teams statistics
url.team.topgoals <- "https://www.fifa.com/worldcup/statistics/teams/goal-scored"
url.team.attempts <- "https://www.fifa.com/worldcup/statistics/teams/shots"
url.team.disciplinary <- "https://www.fifa.com/worldcup/statistics/teams/disciplinary"
  #players statistics
url.player.main <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Data/PlayerList.csv"
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

df.player.scored <- read_html(url.player.scored) %>%
                      html_nodes("table") %>%
                      .[1] %>%
                      html_table(fill=TRUE) %>%
                      .[[1]]
colnames(df.player.scored) <- c('rank.scored','player','goal.scored','goal.assist','mins.player','match.play','penalties.score','left.scored','right.scored','head.scored')

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
