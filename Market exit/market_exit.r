library(dbplyr)
library(dplyr)
library(tidyverse)
library(magrittr)
#install.packages('sqldf')
library(sqldf)
library(tidytext)
#install.packages('wordcloud')
library(wordcloud)
library(reshape2)
#install.packages('syuzhet')
library(syuzhet)
#install.packages('textdata')
library(textdata)
library(ggplot2)
library(gridExtra)

getwd()
setwd("~/Mangala_Local/DSBA_6211/Project/Consolidated")

df_jan <-read.csv('jan_listings.csv')
df_feb <-read.csv('feb_listings.csv')
df_mar <-read.csv('mar_listings.csv')
df_apr <-read.csv('apr_listings.csv')
df_may <-read.csv('may_listings.csv')
df_june <-read.csv('june_listings.csv')

df_jan$last_review <-  as.Date(df_jan$last_review)
df_jan$first_review <- as.Date(df_jan$first_review)
df_feb$last_review <-  as.Date(df_feb$last_review)
df_feb$first_review <- as.Date(df_feb$first_review)
df_mar$last_review <-  as.Date(df_mar$last_review)
df_mar$first_review <- as.Date(df_mar$first_review)
df_apr$last_review <-  as.Date(df_apr$last_review)
df_apr$first_review <- as.Date(df_apr$first_review)
df_may$last_review <-  as.Date(df_may$last_review)
df_may$first_review <- as.Date(df_may$first_review)
df_june$last_review <-  as.Date(df_june$last_review)
df_june$first_review <- as.Date(df_june$first_review)




str(df_jan)
summary(df_jan)

new_df_jan<-sqldf("select id, 
                case 
                  when has_availability='t' then 1
                  when has_availability='f' then 0
                end as availability,
              availability_30,availability_60,availability_90,availability_365,
              number_of_reviews,number_of_reviews_ltm,
              first_review,last_review,
              (last_review-first_review) as lenreview,
              review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
              review_scores_checkin,review_scores_communication,review_scores_location,
              review_scores_value,instant_bookable
              from df_jan
              ")

new_df_jan

new_df_feb<-sqldf("select id, 
                  case 
                  when has_availability='t' then 1
                  when has_availability='f' then 0
                end as availability,
              availability_30,availability_60,availability_90,availability_365,
              number_of_reviews,number_of_reviews_ltm,
              first_review,last_review,
              (last_review-first_review) as lenreview,
              review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
              review_scores_checkin,review_scores_communication,review_scores_location,
              review_scores_value,instant_bookable
              from df_feb")

new_df_mar<-sqldf("select id, 
                case 
                  when has_availability='t' then 1
                  when has_availability='f' then 0
                end as availability,
               availability_30,availability_60,availability_90,availability_365,
              number_of_reviews,number_of_reviews_ltm,
              first_review,last_review,
              (last_review-first_review) as lenreview,
              review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
              review_scores_checkin,review_scores_communication,review_scores_location,
              review_scores_value,instant_bookable
              from df_mar
              ")

new_df_apr<-sqldf("select id, 
                case 
                  when has_availability='t' then 1
                  when has_availability='f' then 0
                end as availability,
              availability_30,availability_60,availability_90,availability_365,
              number_of_reviews,number_of_reviews_ltm,
              first_review,last_review,
              (last_review-first_review) as lenreview,
              review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
              review_scores_checkin,review_scores_communication,review_scores_location,
              review_scores_value,instant_bookable
              from df_apr
              ")

new_df_may<-sqldf("select id, 
                  case 
                      when has_availability='t' then 1
                      when has_availability='f' then 0
                  end as availability,
                availability_30,availability_60,availability_90,availability_365,
                number_of_reviews,number_of_reviews_ltm,
                first_review,last_review,
                (last_review-first_review) as lenreview,
                review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
                review_scores_checkin,review_scores_communication,review_scores_location,
                review_scores_value,instant_bookable
                from df_may
              ")

new_df_june<-sqldf("select id, 
              case 
                  when has_availability='t' then 1
                  when has_availability='f' then 0
                end as availability,
              availability_30,availability_60,availability_90,availability_365,
              number_of_reviews,number_of_reviews_ltm,
              first_review,last_review,
              (last_review-first_review) as lenreview,
              review_scores_rating,review_scores_accuracy,review_scores_cleanliness,
              review_scores_checkin,review_scores_communication,review_scores_location,
              review_scores_value,instant_bookable
              from df_june
              ")



new_df_jan$month<-"Jan"
new_df_feb$month<-"Feb"
new_df_mar$month<-"Mar"
new_df_apr$month<-"Apr"
new_df_may$month<-"May"
new_df_june$month<-"June"


new_data<-rbind(new_df_jan,new_df_feb,new_df_mar,
                new_df_apr,new_df_may,new_df_june)

new_data$line <- 1:nrow(new_data) 
new_data$line<-as.character(new_data$line)

library(ggplot2)
library(survival)


km1 <- survfit(Surv(lenreview,availability)~1, data=new_data)
summary(km1)
plot(km1, xlab = "Length of Review", ylab = "Survival Probability for a Property")


# A special visulization package from github
#install.packages('devtools', dependencies = TRUE)

library(devtools)
#devtools::install_github('sachsmc/ggkm')
library(ggkm)


# To test all independent variables

attach(new_data)
X <- cbind(availability_30,availability_90,availability_365,
           number_of_reviews,number_of_reviews_ltm,
           first_review,last_review,
           review_scores_rating,review_scores_accuracy,
           review_scores_cleanliness,review_scores_checkin,
           review_scores_communication,review_scores_location,
           review_scores_value,instant_bookable)
coxph <- coxph(Surv(lenreview,availability)~X, method="breslow",data=new_data)
summary(coxph)



