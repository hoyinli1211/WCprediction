

attack_rating <- function(cty) {
  
  df <- df.result2 %>% 
          filter(home_team==cty | away_team==cty) %>%
          mutate(attack=ifelse(home_team==cty, home_score,away_score),
                 prodsum=attack*weight)
  return(sum(df$prodsum)/sum(df$weight))
}

defence_rating <- function(cty) {
  
  df <- df.result2 %>% 
    filter(home_team==cty | away_team==cty) %>%
    mutate(defence=ifelse(home_team==cty, away_score, home_score),
           prodsum=defence*weight)
  return(sum(df$prodsum)/sum(df$weight))
}

attack_rating2 <- function(cty) {
  
  df <- df.result2 %>% 
          filter(home_team==cty | away_team==cty) %>%
          mutate(attack=ifelse(home_team==cty, home_score,away_score),
                 finalweight=weight*weight2,
                 prodsum=attack*finalweight)
  return(sum(df$prodsum)/sum(df$finalweight))
}

defence_rating2 <- function(cty) {
  
  df <- df.result2 %>% 
    filter(home_team==cty | away_team==cty) %>%
    mutate(defence=ifelse(home_team==cty, away_score, home_score),
                 finalweight=weight*weight2,
                 prodsum=attack*finalweight)
  return(sum(df$prodsum)/sum(df$finalweight))
}



sapply(v.cty,function(x) attack_rating(x))
sapply(v.cty,function(x) defence_rating(x))
sapply(v.cty,function(x) attack_rating2(x))
sapply(v.cty,function(x) defence_rating2(x))
