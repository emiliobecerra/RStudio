#3.1 Load the Data
HeightWeightSleep <- read.csv("~/Downloads/EconAnalyticsDataSets/HeightWeightSleep.csv")

View(HeightWeightSleep)
summary(HeightWeightSleep)

#3.2 Find Outliers in Sleep and Remove Them

library(tidyverse)
HeightWeightSleep %>% filter(sleep >= 80, sleep <= 100)
HeightWeightSleep %>% filter(sleep >= 90, sleep <= 100)
## There looks to be 257 entries where sleep hours are 97,98,99. These are clearly codes.

## Create new data frame without outliers
New_HWS <- HeightWeightSleep %>%  filter(sleep >= 0, sleep <=90) 
view(New_HWS)

#3.3 Filter and create a new subsample to only include 18-24 year-olds

CollegeHWS <- New_HWS %>% filter(age >= 18, age <=24)
view(CollegeHWS)


#3.3 Compute the mean Age

rm(New_HWS)
rm(HeightWeightSleep)

CollegeHWS$age = as.numeric(as.character(CollegeHWS$age)) 
mean(CollegeHWS$age)
## The mean age for college aged individuals between 18 and 24 is 21.27 or simply 21 years old.


#3.4 Compute the Mean and Median for Sleep and interpret

CollegeHWS$sleep = as.numeric(as.character(CollegeHWS$sleep)) 
mean(CollegeHWS$sleep)
median(CollegeHWS$sleep)
##The sample average sleep hours for college aged individuals (18-24) is 7.36055
##The sample median sleep hours for college aged individuals (18-24) is 7.

#3.5 Create a Histogram for Sleep and Interpret the Dispersion.

hist(CollegeHWS$sleep, 8, 
     main="Sleep Hours Among College-Aged Indidivuals", 
     xlab="sleep hours", ylab="Counts")
## The dispersion of the histogram suggests that the center is located where the mean and median indicate, and the histogram looks to be symmetrical. 

#3.6 Find the Correlation between sleep and sex

cor(CollegeHWS$sleep, CollegeHWS$sex)
## The Correlation is -0.002. It is close to zero so we can guess it will look horizontal indicating almost no correlation.

#3.7 Plot the Correlation

plot(CollegeHWS$sleep, CollegeHWS$sex, type="p", xlim=range(0,20), 
     ylim=range(0,20), 
     main="Relationship Between Sleep Hours and Sex (18-24 year olds)", 
     xlab="Sleep Hours", ylab="Sex")

##Jitter

jsleep=jitter(CollegeHWS$sleep, factor=1)
jsex=jitter(CollegeHWS$sex, factor=1)

##New Jittered Plot

plot(jsleep, jsex, type="p", xlim=range(0,20), ylim=range(0,20), 
     main="Relationship Between Sleep Hours and Sex (18-24 year olds)(Jittered)", 
     xlab="Sleep Hours", ylab="Sex")

#4.0 Online Data!

#4.1 ScoobyDoo data

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scoobydoo <- tuesdata$scoobydoo

#4.2 Summary of Data
summary(scoobydoo)
view(scoobydoo)

#4.3
#We're going to plot episode and imbd ratings to see how good ratings were over time from the first episode to the latest episode aired (1969-2020)
## We want to remove the imbd ratings that have "NULL" and movies or specials to only look at the episodes.

scoobydoo2 <- scoobydoo %>% 
  filter(scoobydoo$index < 587) %>% 
  filter(scoobydoo2$season <= 4)
view(scoobydoo2)

## Episode number is listed as index(1,2,...,574), with index 1 being the first episode ever aired.
plot(scoobydoo2$index, scoobydoo2$imdb, type="p", xlim=range(0,574), 
     ylim=range(4,11), 
     main="Ratings Across All Episodes Aired (1:574 ep., 1969-2020)", 
     xlab="Episode Number", ylab="Rating")

## The plot shows a drastic decrease in ratings around episode numbers ~~ 390:430 which were years 2006-2008. 
## This was around the time The CW was producing ScoobyDoo. 