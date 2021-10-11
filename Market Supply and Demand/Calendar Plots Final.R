
#install.packages('flexdashboard')
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('lubridate')
#install.packages('ggthemes')
#devtools::install_github("jayjacobs/ggcal") #install.packages('ggcal')
#install.packages('plotly')
#install.packages('sf')
#install.packages('tmap')
#install.packages('DT')
#install.packages('readr')


library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)
#install.packages('ggcal')
library(ggcal)
library(plotly)
library(sf)
library(tmap)
library(DT)
library(readr)
library(stringr)

path <- "~/Dropbox/MBAD 6211/rStudio/GroupProject/Data"

read_csv(file.path(path, "calendarTot3.csv")) %>% 
  mutate(price = as.numeric(str_extract(price,'[0-9.]+'))) -> 
  calendar 
read_csv(file.path(path, "listings_test_1.csv")) %>% 
  st_as_sf(coords=c('longitude','latitude'),crs=4326) %>%
  mutate(FullyBooked=availability_365==0) -> 
  listings
calendar$available <- as.character(calendar$available)
calendar$available [calendar$available == "TRUE"] <- '1'
calendar$available [calendar$available == "FALSE"] <- '0'
calendar$available <- as.numeric(calendar$available)
avail_length <- function(lseq) {
  rl <- rle(lseq)
  rl <- rl$lengths[rl$values]
  rl <- mean(rl)
  ifelse(is.nan(rl),0,rl)
}

calendar %>% group_by(listing_id) %>% 
  summarise(al = sum(available)) -> avail

listings %>% 
  left_join(avail,by=c("id"="listing_id")) ->
  listings

calendar %>%
  mutate(mnth = format(date, "%m"),dow=wday(date,label=TRUE)) %>% 
  group_by(mnth,dow,listing_id) %>% 
  summarise(ct=sum(available)) %>% 
  group_by(listing_id) %>% 
  filter(sum(ct)!=0) %>% 
  mutate(dowprop = ct/sum(ct)) %>% 
  select(-ct) %>% 
  spread(key=dow,value=dowprop)  %>% 
  {. ->> tmp} %>% 
  .[,-1] %>% 
  prcomp %>%
  .$x %>% 
  as_data_frame %>% 
  mutate(listing_id=tmp$listing_id) %>%
  left_join(tmp) -> week_use
#listings %>% left_join(week_use,by=c('id'='listing_id')) %>% filter(!is.na(PC1)) -> onk
#tm_shape(onk) + tm_dots(col='PC1',size=0.25)



calendar %>% count(date,wt=available) %>% 
  rename(`Available Properties`=n,Date=date) %>%
  mutate(`Week Day`=wday(Date,label=TRUE),Week=week(Date))  -> res 
ggcal(res$Date,res$`Available Properties`) + scale_fill_viridis_c()

res %>% 
  filter(Week < 53) %>%
  ggplot(aes(x=Week,y=`Available Properties`,fill=`Week Day`)) +    
  geom_col(position='fill') + scale_fill_hue()

res %>% ggplot(aes(x=Date,y=`Available Properties`)) + geom_line(col='red')

calendar %>% count(date,wt=!available) %>% 
  rename(`Unavailable Properties`=n,Date=date) %>%
  mutate(`Week Day`=wday(Date,label=TRUE),Week=week(Date))  -> res 
ggcal(res$Date,res$`Unavailable Properties`) + scale_fill_viridis_c() 

res %>% 
  filter(Week < 53) %>%
  ggplot(aes(x=Week,y=`Unavailable Properties`,fill=`Week Day`)) +    
  geom_col(position='fill') + scale_fill_hue() 

res %>% ggplot(aes(x=Date,y=`Unavailable Properties`)) + geom_line(col='red')

tmap_mode('view')
tm_shape(listings) + tm_dots(col="al",title="Mean Availability Interval",style='fixed',breaks=c(0,1,35,106,232,366))

tmap_mode('view')
tm_shape(listings) + tm_dots(col="FullyBooked",title="Unavailable 365 Days")

avail %>% 
  rename(`Mean Availability Length`=al) %>%
  ggplot(aes(x=`Mean Availability Length`)) + 
  geom_dotplot(binwidth=1,
               method='histodot',
               stackdir="center",
               col='darkblue')

listings %>% filter(row_number()<50) %>% as.data.frame %>% select(name,host_name,neighbourhood,room_type,price,minimum_nights,availability_365) %>% as_data_frame -> l2
datatable(l2)
