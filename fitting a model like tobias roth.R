#This script is to get data for my stan model
setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo")
library(raster)
library(dplyr)
library(tidyverse)
library(sf)
library(RANN)

all_data<-read.csv("Final_Nest_Monitoring_EABA&COAT_ALL_years.csv")

east_bay_only_data <- subset(all_data, site== "East Bay Mainland")

#nest, my response variable is nest fate (success or failed):
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

#the last day of the nest depends on if it's alive or dead (success or failed)
#if alive -> then it's the hatch day = Hatch_Date
#if dead, then it's the Last.Active day. 

#so, let's make that one variable
east_bay_only_data$last_observed <- ifelse(east_bay_only_data$Fate == "success",
                                           east_bay_only_data$Hatch_Date, east_bay_only_data$Last.Active)
View(east_bay_only_data)
east_bay_only_data$last_observed

#NOTE THAT I HAVE AN IRREGULAR NEST CONTROL BECAUSE WE DON'T CHECK EVERY DAY
#for now, I'm ignoring that, but later I need to do the irregular model
#wait, it seems that there's a Mayfield end date formula -- so they already did this stuff for me - Yay!

#The Mayfield days formula (Mayfield_days..formula.) gives the number of days the nest was active, I believe

#Maxage in the tobias roth example is described as "maximum of last"

maxage <- east_bay_only_data$Mayfield_days..formula.
#this made a chr variable
#so lets set this to an interger
str(maxage)
int_maxage <- as.integer(maxage)

#so age should be length int_maxage...

str(east_bay_only_data)

#okay, I'm not going to get anywhere until I understand what is "age" in tobias roth -- why is age -1.65, -1.54, etc. 

