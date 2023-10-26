#This script is to get data for my stan model

setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo")
library(raster) # you may want to switch to using the terra package
# https://rspatial.org/pkg/1-introduction.html
# e.g., https://oceanhealthindex.org/news/raster_to_terra/
# library(dplyr) # dplyr is part of the tidyverse, so you don't need to load it directly
library(tidyverse)
library(sf)
library(RANN)

all_data<-read.csv("raw_data/Final_Nest_Monitoring_EABA&COAT_ALL_years.csv")

east_bay_only_data <- subset(all_data, site== "East Bay Mainland")

#my response variable is nest fate (success or failed):
#so I need to clean that variable -- remove unknowns

clean_nest_fate_data <- east_bay_only_data %>% # two assignment "<-" operators doesn't make sense
  filter(Fate %in% c("success", "failed"))

# # suggest you move this number of nest calculation to the end, after you've prepared all other variables
# # in case there are missing data in the other variables that might require dropping some nests
# #first I need to know the number of nests
# Nnest<-nrow(clean_nest_fate_data)
# str(Nnest)

#I want to create a model that is like tobiasroth.github.io/BDAEcology
#so I need indexing tools  -- first observation, last observation, maxage, etc.
# the model is a "known fate" model that estimates daily survival conditional on the
# nest being "alive" the previous day. Therefore, the key data object is a matrix of
# 1 and 0 with a row for each nest, a column for each day of the season (within any given year)
# and values of 1 for days that we know any given nest was alive and 0 everywhere else.
# So, I think for this model, the dates we need are in the Mayfield date columns

##### measuring days in a more intuitive way
### the package lubridate https://lubridate.tidyverse.org/
### provides a lot of useful and more intuitive functions for handling dates
clean_nest_fate_data$start_date <- lubridate::dmy(clean_nest_fate_data$Mayfield_start_date..formula.)
clean_nest_fate_data$end_date <- lubridate::dmy(clean_nest_fate_data$Mayfield_end.date..formula.)



### lubridate also includes functions that allow for calculating ordinal days - number of days from the start of the year
### I think this integer treatment is what you need. It creates day values
### that have an intuitive scaling. This will also ensure that any date treatment doesn't accidentally
### count days from the previous year, and that incorrect dates will stand out
# defining a homebrew function to calculate ordinal day of the year with two lubridate functions
ordinal_date_function <- function(x){
  oday <- lubridate::yday(lubridate::as_date(x))
}

clean_nest_fate_data$start_date_ordinal <- ordinal_date_function(clean_nest_fate_data$start_date)
range(clean_nest_fate_data$start_date_ordinal)
range(clean_nest_fate_data$start_date_ordinal,
      na.rm = TRUE) # there are quite a few NA values that will need to be removed.
### NA's are due to blank spaces in the excel file. - missing data



clean_nest_fate_data$end_date_ordinal <- ordinal_date_function(clean_nest_fate_data$end_date)
range(clean_nest_fate_data$end_date_ordinal)
range(clean_nest_fate_data$end_date_ordinal,
      na.rm = TRUE) # there are quite a few NA values that will need to be removed.
### but before removing NAs, we need to understand what they represent.


earliest_ordinal_day <- min(clean_nest_fate_data$start_date_ordinal, na.rm = TRUE) -1
## earliest_ordinal_day (min() -1) is the base-date by which you can set the start of the season
## this mutate call below sets all dates to an ordinal day from the start of the season
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(start_date_ordinal = start_date_ordinal - earliest_ordinal_day,
         end_date_ordinal = end_date_ordinal - earliest_ordinal_day)



# covariate ---------------------------------------------------------------

clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(eggs_hatched = as.integer(X._of_Eggs_Hatched))

range(clean_nest_fate_data$eggs_hatched,na.rm = TRUE)

### removing nests with NA values in the start and end dates and the eggs_hatched column
clean_nest_fate_data <- clean_nest_fate_data %>%
  filter(!is.na(start_date_ordinal),
         !is.na(end_date_ordinal),
         !is.na(eggs_hatched))

range(clean_nest_fate_data$eggs_hatched)



range(clean_nest_fate_data$start_date_ordinal)
range(clean_nest_fate_data$end_date_ordinal)
# [1]  14 128 ## the 128 is because of an error in the end date -- apparently the nest end_date is october 6, 1952!
hist(clean_nest_fate_data$end_date_ordinal)
## other than that one value all others are less than 60

clean_nest_fate_data <- clean_nest_fate_data %>%
  filter(end_date_ordinal < 60) %>%
  mutate(eggs_hatched_scaled = as.numeric(scale(eggs_hatched))) # scaling the predictor (mean = 0, sd = 1)

View(clean_nest_fate_data$eggs_hatched_scaled)


maxage <- max(clean_nest_fate_data$end_date_ordinal)
nNests <- nrow(clean_nest_fate_data)
## now we have all of the information necessary to build the main data matrix
## of the y[nNests,maxday]

y <- matrix(data = 0,
            nrow = nNests,
            ncol = maxage) # pre-filling the entire matrix with 0 values
## here's a nested loop to fill in the 1-values in the y matrix (there are more concise ways of doing this, but
## sometimes it's nice to make the code super clear)
for(i in 1:nNests){
  first = clean_nest_fate_data[i,"start_date_ordinal"]
  last = clean_nest_fate_data[i,"end_date_ordinal"]

  for(j in first : (last-1)){
    y[i,j] <- 1 # setting values to 1 for each nest on the days between first and last
  }
}

## check to make sure this worked
ndays_matrix <- rowSums(y)
ndays_df <- as.integer(clean_nest_fate_data$Mayfield_days..formula.)

range(ndays_df - ndays_matrix, na.rm = TRUE)
length(which(ndays_df - ndays_matrix == 0))
length(which(ndays_df - ndays_matrix != 0))
## there are some where this calculation doesn't match up.
## worth checking why for the ~ 120 or so nests.


#this code is to check why there are mismatches
differences<- ndays_df - ndays_matrix !=0
mismatched_data <- clean_nest_fate_data[differences, ]
print(mismatched_data)
View(mismatched_data)
# Subset ndays_df and ndays_matrix where differences are not equal to 0
mismatched_ndays_df <- ndays_df[differences]
mismatched_ndays_matrix <- ndays_matrix[differences]

# Create a data frame to display the differences
mismatched_values <- data.frame(
  ndays_df = mismatched_ndays_df,
  ndays_matrix = mismatched_ndays_matrix
)
# Display the data frame with the mismatched values
View(mismatched_values)


# making the Stan data list -----------------------------------------------


## Stan requires a named list as a data object

  stan_data <- list(Nnests = nNests,
                    first_day_as_int_days = clean_nest_fate_data$start_date_ordinal,
                    last_day_as_int_days = clean_nest_fate_data$end_date_ordinal,
                    maxage = maxage,
                    y = y,
                    x_of_eggs = clean_nest_fate_data$eggs_hatched_scaled)


str(stan_data)


#now save this as an .rds
saveRDS(stan_data, "stan_data_list.rds")



#
# #by floating the eggs, we estimate the day the nest was initiated
# #this is called Estimated.Nest.Initiation.Date
# str(clean_nest_fate_data$Estimated.Nest.Initiation.Date)
# #this is not an integer
# #Turn it into an integer
# #the following code counts the number of seconds since 1/1/1970 - true, it's also just the standard date unit in base-R
# clean_nest_fate_data$date_init=as.POSIXct(clean_nest_fate_data$Estimated.Nest.Initiation.Date, format = "%d-%b-%Y")
#
#
# clean_nest_fate_data$first_day_as_int_sec<- as.integer(clean_nest_fate_data$date_init)
# View(clean_nest_fate_data$first_day_as_int)
# #to view this as number of days I need to divide by 86400
# #I use the floor function to round this to a whole number (without this, each value is .17 e.g. 10412.17)
# clean_nest_fate_data$first_day_as_int_days <- floor(as.integer(clean_nest_fate_data$first_day_as_int_sec) / 86400)
# View(clean_nest_fate_data$first_day_as_int_days)
# #this is days since 1/1/1970
# #there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
# clean_nest_fate_data$first_day_as_int_days[is.na(clean_nest_fate_data$first_day_as_int_days) ] <- -1
#
#
# #the last day of the nest depends on if it's alive or dead (success or failed)
# #if alive -> then it's the hatch day = Hatch_Date
# #if dead, then it's the Last.Active day.
#
# #so, let's make that one variable
# clean_nest_fate_data$last_observed <- ifelse(clean_nest_fate_data$Fate == "success",
#                                            clean_nest_fate_data$Hatch_Date, clean_nest_fate_data$Last.Active)
#
# clean_nest_fate_data$last_observed
# str(clean_nest_fate_data$last_observed)
#
#
# #make it an integer
# #the following code counts the number of seconds since 1/1/1970
# clean_nest_fate_data$last_day=as.POSIXct(clean_nest_fate_data$last_observed, format = "%d-%b-%Y")
#
# clean_nest_fate_data$last_day_ordinal_day <- ordinal_date_function(clean_nest_fate_data$last_day)
# range(clean_nest_fate_data$last_day_ordinal_day)
# range(clean_nest_fate_data$last_day_ordinal_day,
#       na.rm = TRUE) # there are quite a few NA values that will need to be removed.
#
# clean_nest_fate_data$last_day_as_int<- as.integer(clean_nest_fate_data$last_day)
# View(clean_nest_fate_data$last_day_as_int)
# str(clean_nest_fate_data$last_day_as_int)
# #to view this as number of days I need to divide by 86400
# clean_nest_fate_data$last_day_as_int_days <- floor(as.integer(clean_nest_fate_data$last_day_as_int) / 86400)
# View(clean_nest_fate_data$last_day_as_int_days)
# #this is days since 1/1/1970
#
# #there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
# clean_nest_fate_data$last_day_as_int_days[is.na(clean_nest_fate_data$last_day_as_int_days) ] <- -1
#
# ## removing the NA values the dplyr (part of the tidyverse family of packages)
#
#
#
# #NOTE THAT I HAVE AN IRREGULAR NEST CONTROL BECAUSE WE DON'T CHECK EVERY DAY
# #for now, I'm ignoring that, but later I need to do the irregular model
# #wait, it seems that there's a Mayfield end date formula -- so they already did this stuff for me - Yay!
#
# #The Mayfield days formula (Mayfield_days..formula.) gives the number of days the nest was active, I believe
# mayfield_days_formula<-clean_nest_fate_data$Mayfield_days..formula.
# View(mayfield_days_formula)
# str(mayfield_days_formula)
#
# #is it the same as last-first?
# clean_nest_fate_data$total_days_nest_active<-clean_nest_fate_data$last_day_as_int_days - clean_nest_fate_data$first_day_as_int_days
# View(clean_nest_fate_data$total_days_nest_active)
# #it is not the same. Probably because the mayfield formula isn't an exact count -- I'm guessing it takes into acount the interval betweeen checks
# #so the Mayfield formula may be suitable for an irregular nest control model -- but for now let's assume a regular nest control.
#
# #this was giving me "numeric(0)" so something went wrong.
# #stan does not like NAs so I drop them
# #leaving this here in case I want it later
# #clean_nest_fate_data <- clean_nest_fate_data[complete.cases(clean_nest_fate_data), ]
# #View(clean_nest_fate_data$total_days_nest_active)
#
#
# #must make it a numeric variable, because there are values like 1.5 -> so not an integer
# clean_nest_fate_data$Mayfield_days..formula.<-as.numeric(clean_nest_fate_data$Mayfield_days..formula.)
# #there are NA's which I think STAN does not like.
# #so I'm just going to ignore missing values
# clean_nest_fate_data[!is.na(clean_nest_fate_data$X._of_Eggs_Hatched), ]
# str(clean_nest_fate_data$Mayfield_days..formula.)
#
# #okay, I'm not going to get anywhere until I understand what is "age" in tobias roth -- why is age -1.65, -1.54, etc
# #answer, I believe it is standardized, that's why it can be negative etc.>
#
# #their maxage is an integer with the maximum number of days alive
# #to do this with the Mayfield formula:
# #max(clean_nest_fate_data$Mayfield_days..formula., na.rm=TRUE)
# #maxage <- as.integer(38)
# #str(maxage)
#
# maxage<-max(clean_nest_fate_data$last_day_as_int_days, na.rm=TRUE)
# View(maxage)
#
# #Their y variable seems to be a measure of if the nest is alive or not - probably per day
# #hmmm, don't really understand that
# str(clean_nest_fate_data$Fate)
# clean_nest_fate_data$y <- clean_nest_fate_data$Fate
# #stan does not deal with categorical variables, so make it a 0,1 binary
# clean_nest_fate_data$y_numeric <- ifelse(clean_nest_fate_data$y == "success", 1, 0)
# str(clean_nest_fate_data$y_numeric)
#
# #I'm just going to keep it simple and add one predictor variable -- number of eggs hatched
# #this should be correlated with the length of time the nest was active
# #nests that were successful will be active longer, will hatch more eggs
# str(clean_nest_fate_data$X._of_Eggs_Hatched)
#
# clean_nest_fate_data$X._of_Eggs_Hatched<-as.integer(clean_nest_fate_data$X._of_Eggs_Hatched)
# str(clean_nest_fate_data$X._of_Eggs_Hatched)
# #there's NA's  - so I use in<lower=0> in my stan model. So any negative value will not affect the model, I think
# clean_nest_fate_data$X._of_Eggs_Hatched[is.na(clean_nest_fate_data$X._of_Eggs_Hatched)] <- -1
# str(clean_nest_fate_data$X._of_Eggs_Hatched)
#
# #I'm renaming the variable here
# clean_nest_fate_data$x_of_eggs <- clean_nest_fate_data$X._of_Eggs_Hatched
# View(clean_nest_fate_data$x_of_eggs)
#


# #now save this as an .rds
# saveRDS(clean_nest_fate_data, "preprocessed_data_like_tobias.rds")

