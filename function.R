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
                 finalweight=weight*weight1,
                 prodsum=attack*finalweight)
  return(sum(df$prodsum)/sum(df$finalweight))
}

defence_rating2 <- function(cty) {
  
  df <- df.result2 %>% 
    filter(home_team==cty | away_team==cty) %>%
    mutate(defence=ifelse(home_team==cty, away_score, home_score),
                 finalweight=weight*weight1,
                 prodsum=defence*finalweight)
  return(sum(df$prodsum)/sum(df$finalweight))
}

matchresult <- function(cty1, cty2) {

   attack.cty1 = attack_rating2(cty1)
   defence.cty1 = defence_rating2(cty1)
   attack.cty2 = attack_rating2(cty2)
   defence.cty2 = defence_rating2(cty2)
  
   home.score = rpois(1,attack.cty1*defence.cty2)
   away.score = rpois(1,attack.cty2*defence.cty1)
  
   return(final.result = paste0(home.score,'-',away.score))
}

simulation.matchresult <- function(df,n, cty1, cty2) {
  
  for (i = 1:n) {
    df <- df %>% bind_rows_(matchresult,cty1,cty2)
  }
  return(df)
  
}
#sapply(v.cty,function(x) attack_rating(x))
#sapply(v.cty,function(x) defence_rating(x))
#sapply(v.cty,function(x) attack_rating2(x))
#sapply(v.cty,function(x) defence_rating2(x))


