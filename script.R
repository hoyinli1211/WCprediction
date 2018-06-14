#2017-07-09 Packages handling
list.of.packages <- c("lubridate", "ggplot2", "plyr", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#lubridate
#transform different format of date to standard yyyy-mm-dd format
date1 <- dmy("01-02-2010")
date2 <- mdy("01-02-2010")
date3 <- ymd("20100102"ß)
date4 <- dmy("09072017")
date5 <- dmy("02-Mar-2010")
date6 <- dmy("02/03/2010")
now()
today()
#
wday(date4) #day of week; 1: Sunday; 2: Monday; ... 7: Saturday;
yday(date4) #day of year
mday(date4) #day of month
#calculate time/day difference
as.numeric(difftime(date1, date2, units="day"))
date2 + ddays(3)   #dyears, dweeks, ddays, dhours, dminitues, dseconds


date <- today()
update(date, year=2010, month=11, wday=5)

fwd <- rep(today(),12)
year(fwd) <- rep(2016, 12)
month(fwd) <- c(1:12)
day(fwd) <- 1
ifelse(wday(fwd)==1, fwd + ddays(1), fwd)


#plyr
baberuth <- subset(baseball, id=="ruthba01")
baberuth <- transform(baberuth, cyear = year - min(year) + 1)
baseball <- ddply(baseball, .(id), transform, cyear = year - min(year) + 1)


#testing
mcc <- c('8','9','10','9999','9999')
ctyc <- c('999','2','3','4','999')
value <- c(1:5)
dfM <- data.frame(mcc, ctyc, value)

xmcc <- as.character(c(8:12))
xctyc <- as.character(c(1:4,12))
df <- data.frame(xmcc, xctyc,stringsAsFactors=FALSE)
colnames(df) <- c("mcc", "ctyc")


df$mcc <- ifelse(df$mcc %in% mcc, df$mcc, "9999")
df$ctyc <- ifelse(df$ctyc %in% ctyc, df$ctyc, "999")

merge(x=df, y=dfM, by=c('mcc','ctyc'), all = TRUE)


#data import - 13-May-2018
  #read csv from online
path <- 'http://ia801504.s3dns.us.archive.org/opensanctions/worldpresidentsdb.csv'
df <- read.csv(url(path))


  #read csv for ISO country code
library(stringr)
url.cty <- 'https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv'
df.cty <- read.csv(url(url.cty))
#str(df.cty)
df.cty2 <- df.cty[,c('official_name_en','CLDR.display.name','ISO3166.1.Alpha.2','ISO3166.1.Alpha.3','ISO3166.1.numeric','Continent','Dial','Region.Code','Region.Name')]
colnames(df.cty2) <- c('cty','city','cty.cd2','cty.cd3','cty.cdInt','continent','dial','region.cd','region')
df.cty2$cty.cdInt <- str_pad(df.cty2$cty.cdInt,3,pad="0")
View(df.cty2)

  #read csv for ISO merchant category code
url.mcc <- 'https://raw.githubusercontent.com/greggles/mcc-codes/master/mcc_codes.csv'
df.mcc <- read.csv(url(url.mcc))
str(df.mcc)
View(df.mcc)

  #read csv for BIN of issuing bank
    #https://www.freebinchecker.com/
url.bin <- 'https://raw.githubusercontent.com/binlist/data/master/ranges.csv'
df.bin <- read.csv(url(url.bin))
str(df.bin)
df.bin$bin6 <- substr(df.bin$iin_start, 1, 6)
View(df.bin)

  #read json file for payment gateway
library(rjson)
library('tidyverse')
url.paymentGate <- 'https://raw.githubusercontent.com/thephpleague/omnipay/master/composer.json'
df.paymentGate <- fromJSON(file=url.paymentGate)
df.paymentGate <- as.data.table(df.paymentGate)[order(keywords)]
View(df.paymentGate)



# string like comparison 19-May-2018

keywords <- c('ITUNES','GOOGLE')
merchant <- c('www.itunes.com','www.itune.com','www.google.walmart.com','paypal')

result <- grep(paste(keywords,collapse="|"), 
     toupper(merchant), value=TRUE)

# read xml from url
library(XML)
url <- "https://www.censtatd.gov.hk/fd.jsp?file=X1060001012018MM03B0100.zip&product_id=X1060001"
temp <- tempfile()
download.file(url, temp, quiet = T)
unzip(temp)
filename <- paste0(getwd(),"/CPI_Masterdata.xml")
df <- xmlParse(filename)
df <- xmlToList(df)
str(df)
View(df)
