url.src1 <- 'https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/Script.R'
source(url.src1)

url.src2 <- 'https://raw.githubusercontent.com/hoyinli1211/WCprediction/master/function.R'
source(url.src2)

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
