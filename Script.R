################################
# DBS World Cup competition
  #2018-06-14
################################

################################
  #Package management
################################
list.of.packages <- c("tidyverse","stringr", "rvest")
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
df.schedule1 <- read.csv(file=url1)
df.schedule2 <- read.csv(file=url2)
View(df.schedule1)
View(df.schedule2)

v.cty <- as.character(df.schedule2$team)

#historical match result
url3 <- "https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/historical.csv"
df.result <- read.csv(url3)
df.result2 <- df.result %>%
                filter(home_team %in% v.cty | away_team %in% v.cty)


