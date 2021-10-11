#install.packages('lubridate')
#install.packages("dplyr")

library(lubridate)
library(dplyr)

path <- "~/Dropbox/MBAD 6211/rStudio/GroupProject/Data"

dfJan <- read.csv(file.path(path, "listings_jan.csv"), header=TRUE)
dfFeb <- read.csv(file.path(path, "listings_feb.csv"), header=TRUE)
dfMar <- read.csv(file.path(path, "listings_mar.csv"), header=TRUE)
dfApr <- read.csv(file.path(path, "listings_apr.csv"), header=TRUE)
dfMay <- read.csv(file.path(path, "listings_may.csv"), header=TRUE)
dfJun <- read.csv(file.path(path, "listings_jun.csv"), header=TRUE)
dfJul <- read.csv(file.path(path, "listingsJul.csv"), header=TRUE)
dfAug <- read.csv(file.path(path, "listings_aug.csv"), header=TRUE)
dfSep <- read.csv(file.path(path, "listings_sep.csv"), header=TRUE)
dfOct <- read.csv(file.path(path, "listings_oct2.csv"), header=TRUE)
dfNov <- read.csv(file.path(path, "listings_nov.csv"), header=TRUE)
dfDec <- read.csv(file.path(path, "listings_dec.csv"), header=TRUE)


#summary(df1)
#str(df1)

#df1 <- merge(dfJan, dfFeb, dfMar, dfApr, dfMay, 
#               dfJun, dfJul,all=TRUE)
df1 <- merge(dfJan, dfFeb,all=TRUE)
df2 <- merge(dfMar, dfApr,all=TRUE)
df3 <- merge(dfJun, dfJul,all=TRUE)
df6 <- merge(df1, df2,all=TRUE)
df7 <- merge(df2,dfMay,all=TRUE)
df7<- merge(df6, df7, all=TRUE)
dfTot<-merge(df7,df3,all=TRUE)
#, dfAug, dfSep, dfOct, dfNov, dfDec)

#dfTot <- dfTot[order(dfTot$listing_id),]

write.csv(dfTot,file.path(path, "listings_test_1.csv"), row.names = TRUE)

