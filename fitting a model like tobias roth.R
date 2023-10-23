#This script is to get data for my stan model

setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo/raw_data")
library(raster) # you may want to switch to using the terra package
# https://rspatial.org/pkg/1-introduction.html
# e.g., https://oceanhealthindex.org/news/raster_to_terra/
library(dplyr)
library(tidyverse)
library(sf)
library(RANN)

all_data<-read.csv("Final_Nest_Monitoring_EABA&COAT_ALL_years.csv")

east_bay_only_data <- subset(all_data, site== "East Bay Mainland")

#my response variable is nest fate (success or failed):
#so I need to clean that variable -- remove unknowns

clean_nest_fate_data <- filtered_fate_data<-east_bay_only_data %>%
  filter(Fate %in% c("success", "failed"))

#first I need to know the number of nests
Nnest<-nrow(clean_nest_fate_data)
str(Nnest)

#I want to create a model that is like tobiasroth.github.io/BDAEcology
#so I need predictor variables -- first observation, last observation, maxage, etc.


#by floating the eggs, we estimate the day the nest was initiated
#this is called Estimated.Nest.Initiation.Date
str(clean_nest_fate_data$Estimated.Nest.Initiation.Date)
#this is not an integer
#Turn it into an integer
#the following code counts the number of seconds since 1/1/1970
clean_nest_fate_data$date_init=as.POSIXct(clean_nest_fate_data$Estimated.Nest.Initiation.Date, format = "%d-%b-%Y")
clean_nest_fate_data$first_day_as_int_sec<- as.integer(clean_nest_fate_data$date_init)
View(clean_nest_fate_data$first_day_as_int)
#to view this as number of days I need to divide by 86400
#I use the floor function to round this to a whole number (without this, each value is .17 e.g. 10412.17)
clean_nest_fate_data$first_day_as_int_days <- floor(as.integer(clean_nest_fate_data$first_day_as_int_sec) / 86400)
View(clean_nest_fate_data$first_day_as_int_days)
#this is days since 1/1/1970
#there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
clean_nest_fate_data$first_day_as_int_days[is.na(clean_nest_fate_data$first_day_as_int_days) ] <- -1


#the last day of the nest depends on if it's alive or dead (success or failed)
#if alive -> then it's the hatch day = Hatch_Date
#if dead, then it's the Last.Active day.

#so, let's make that one variable
clean_nest_fate_data$last_observed <- ifelse(clean_nest_fate_data$Fate == "success",
                                           clean_nest_fate_data$Hatch_Date, clean_nest_fate_data$Last.Active)

clean_nest_fate_data$last_observed
str(clean_nest_fate_data$last_observed)
#make it an integer
#the following code counts the number of seconds since 1/1/1970
clean_nest_fate_data$last_day=as.POSIXct(clean_nest_fate_data$last_observed, format = "%d-%b-%Y")
clean_nest_fate_data$last_day_as_int<- as.integer(clean_nest_fate_data$last_day)
View(clean_nest_fate_data$last_day_as_int)
str(clean_nest_fate_data$last_day_as_int)
#to view this as number of days I need to divide by 86400
clean_nest_fate_data$last_day_as_int_days <- floor(as.integer(clean_nest_fate_data$last_day_as_int) / 86400)
View(clean_nest_fate_data$last_day_as_int_days)
#this is days since 1/1/1970

#there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
clean_nest_fate_data$last_day_as_int_days[is.na(clean_nest_fate_data$last_day_as_int_days) ] <- -1



#NOTE THAT I HAVE AN IRREGULAR NEST CONTROL BECAUSE WE DON'T CHECK EVERY DAY
#for now, I'm ignoring that, but later I need to do the irregular model
#wait, it seems that there's a Mayfield end date formula -- so they already did this stuff for me - Yay!

#The Mayfield days formula (Mayfield_days..formula.) gives the number of days the nest was active, I believe
mayfield_days_formula<-clean_nest_fate_data$Mayfield_days..formula.
View(mayfield_days_formula)
str(mayfield_days_formula)

#is it the same as last-first?
clean_nest_fate_data$total_days_nest_active<-clean_nest_fate_data$last_day_as_int_days - clean_nest_fate_data$first_day_as_int_days
View(clean_nest_fate_data$total_days_nest_active)
#it is not the same. Probably because the mayfield formula isn't an exact count -- I'm guessing it takes into acount the interval betweeen checks
#so the Mayfield formula may be suitable for an irregular nest control model -- but for now let's assume a regular nest control.

#this was giving me "numeric(0)" so something went wrong.
#stan does not like NAs so I drop them
#leaving this here in case I want it later
#clean_nest_fate_data <- clean_nest_fate_data[complete.cases(clean_nest_fate_data), ]
#View(clean_nest_fate_data$total_days_nest_active)


#must make it a numeric variable, because there are values like 1.5 -> so not an integer
clean_nest_fate_data$Mayfield_days..formula.<-as.numeric(clean_nest_fate_data$Mayfield_days..formula.)
#there are NA's which I think STAN does not like.
#so I'm just going to ignore missing values
clean_nest_fate_data[!is.na(clean_nest_fate_data$X._of_Eggs_Hatched), ]
str(clean_nest_fate_data$Mayfield_days..formula.)

<<<<<<< HEAD
#okay, I'm not going to get anywhere until I understand what is "age" in tobias roth -- why is age -1.65, -1.54, etc.
=======
#okay, I'm not going to get anywhere until I understand what is "age" in tobias roth -- why is age -1.65, -1.54, etc
#answer, I believe it is standardized, that's why it can be negative etc.>

#their maxage is an integer with the maximum number of days alive
#to do this with the Mayfield formula:
#max(clean_nest_fate_data$Mayfield_days..formula., na.rm=TRUE)
#maxage <- as.integer(38)
#str(maxage)

maxage<-max(clean_nest_fate_data$last_day_as_int_days, na.rm=TRUE)
View(maxage)

#Their y variable seems to be a measure of if the nest is alive or not - probably per day
#hmmm, don't really understand that
str(clean_nest_fate_data$Fate)
clean_nest_fate_data$y <- clean_nest_fate_data$Fate
#stan does not deal with categorical variables, so make it a 0,1 binary
clean_nest_fate_data$y_numeric <- ifelse(clean_nest_fate_data$y == "success", 1, 0)
str(clean_nest_fate_data$y_numeric)

#I'm just going to keep it simple and add one predictor variable -- number of eggs hatched
#this should be correlated with the length of time the nest was active
#nests that were successful will be active longer, will hatch more eggs
str(clean_nest_fate_data$X._of_Eggs_Hatched)

clean_nest_fate_data$X._of_Eggs_Hatched<-as.integer(clean_nest_fate_data$X._of_Eggs_Hatched)
str(clean_nest_fate_data$X._of_Eggs_Hatched)
#there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
clean_nest_fate_data$X._of_Eggs_Hatched[is.na(clean_nest_fate_data$X._of_Eggs_Hatched)] <- -1
str(clean_nest_fate_data$X._of_Eggs_Hatched)

#I'm renaming the variable here
clean_nest_fate_data$x_of_eggs <- clean_nest_fate_data$X._of_Eggs_Hatched
View(clean_nest_fate_data$x_of_eggs)



#now save this as an .rds
saveRDS(clean_nest_fate_data, "preprocessed_data_like_tobias.rds")

