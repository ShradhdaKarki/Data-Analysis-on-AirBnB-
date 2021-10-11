#install.packages('lubridate')
#install.packages("dplyr")

library(lubridate)
library(dplyr)

path <- "~/Dropbox/MBAD 6211/rStudio/GroupProject/Data"

df1 <- read.csv(file.path(path, "calendarJan.csv"), na.strings=c("NA",""), header=TRUE)
df2 <- read.csv(file.path(path, "calendarFeb.csv"), na.strings=c("NA",""), header=TRUE)
df3 <- read.csv(file.path(path, "calendarMar.csv"), na.strings=c("NA",""), header=TRUE)
df4 <- read.csv(file.path(path, "calendarApr.csv"), na.strings=c("NA",""), header=TRUE)
df5 <- read.csv(file.path(path, "calendarMay.csv"), na.strings=c("NA",""), header=TRUE)
df6 <- read.csv(file.path(path, "calendarJun.csv"), na.strings=c("NA",""), header=TRUE)
df7 <- read.csv(file.path(path, "calendarJul.csv"), na.strings=c("NA",""), header=TRUE)
df8 <- read.csv(file.path(path, "calendarAug.csv"), na.strings=c("NA",""), header=TRUE)
df9 <- read.csv(file.path(path, "calendarSep.csv"), na.strings=c("NA",""), header=TRUE)
df10 <- read.csv(file.path(path, "calendarOct.csv"), na.strings=c("NA",""), header=TRUE)
df11 <- read.csv(file.path(path, "calendarNov.csv"), na.strings=c("NA",""), header=TRUE)
df12 <- read.csv(file.path(path, "calendarDec.csv"), na.strings=c("NA",""), header=TRUE)


dfJan <- df1[df1$date >= "2020-01-01" & df1$date < "2020-02-13",]
dfFeb <- df2[df2$date >= "2020-02-13" & df2$date < "2020-03-16",]
dfMar <- df3[df3$date >= "2020-03-16" & df3$date < "2020-04-14",]
dfApr <- df4[df4$date >= "2020-04-14" & df4$date < "2020-05-12",]
dfMay <- df5[df5$date >= "2020-05-12" & df5$date < "2020-06-10",]
dfJun <- df6[df6$date >= "2020-06-10" & df6$date < "2020-07-11",]
dfJul <- df7[df7$date >= "2020-07-11" & df7$date < "2020-08-31",]
dfAug <- df8[df8$date >= "2020-08-31" & df8$date < "2020-09-28",]
dfSep <- df9[df9$date >= "2020-09-28" & df9$date < "2020-10-24",]
dfOct <- df10[df10$date >= "2020-10-24" & df10$date < "2020-11-10",]
dfNov <- df11[df11$date >= "2020-11-10" & df11$date < "2020-12-21",]
dfDec <- df12[df12$date >= "2020-12-21" & df12$date < "2021-01-19",]

summary(df1)
str(df1)

dfTot <- rbind(dfJan, dfFeb, dfMar, dfApr, dfMay, 
               dfJun, dfJul)
#, dfAug, dfSep, dfOct, dfNov, dfDec)

dfTot <- dfTot[order(dfTot$listing_id),]

write.csv(dfTot,file.path(path, "calendarTot3.csv"), row.names = TRUE)

