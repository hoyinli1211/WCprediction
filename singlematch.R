x <- simulation.matchresult(10000, 'Serbia', 'Switzerland')

x2 <- x %>% 
  mutate(home=as.numeric(substr(result,1,1)),
         away=as.numeric(substr(result,3,3)),
         result2=ifelse(home==away,'tie',
                          ifelse(home>away,'home','away')))

x3a <- x2 %>% group_by(result) %>% summarise(n=n()) %>% arrange(desc(n))

x3b <- x2 %>% group_by(result2) %>% summarise(n=n()) %>% arrange(desc(n))

View(x3a)
View(x3b)
