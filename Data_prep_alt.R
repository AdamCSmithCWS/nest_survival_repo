


library(tidyverse)
library(sf)
library(units)
library(ggplot2)


## generate full dataset of shorebird nests with known fate
all_data<-read.csv("raw_data/Final_Nest_Monitoring_EABA&COAT_ALL_years.csv")

## code was used to count the number of nests for each species
# sp_w_data <- all_data %>% 
#   group_by(species) %>% 
#   summarise(n_nests = n())
# write_csv(sp_w_data,"species_list.csv")
## then this csv file was edited manually to add a 
## species group column that allows for selecting shorebirds
## gulls etc.
sp_list <- read_csv("species_list.csv")

# below also removes nests with no location
east_bay_only_data <- subset(all_data, site== "East Bay Mainland") %>% 
  left_join(.,sp_list,
            by = "species") %>% 
  select(-n_nests) %>% 
  filter(!is.na(Nest_location_northing_WGS84_Dec_degree), 
         !is.na(Nest_location_Easting_WGS84_Dec_degree))





# Creating full dataset of shorebird nests --------------------------------

#my response variable is nest fate (success or failed):
#so I need to clean that variable -- remove unknowns

## moving much of the data prep steps into a single step
## so that this dataset has only nests with all the necessary
## data

#3 improving this function to calculate ordinal day of the 
## season, 
ordinal_date_function <- function(x, earliest = NULL){
  oday <- lubridate::yday(lubridate::as_date(x))
  if(is.null(earliest)){
    earliest <- min(oday, na.rm = TRUE) -1
  }else{
    earliest <- lubridate::yday(lubridate::as_date(earliest))
    earliest <- min(earliest,na.rm = TRUE)-1
  }
  oday <- oday - earliest
  return(oday)
}

## the two nest location overwrites appear to be typos with coordinates with 53 that should be 63 and 8.7 instead of 80.7
clean_nest_fate_data_shore <- east_bay_only_data %>% 
  mutate(start_date = lubridate::dmy(Mayfield_start_date..formula.),
         end_date = lubridate::dmy(Mayfield_end.date..formula.),
         start_date_ordinal = ordinal_date_function(start_date),
         end_date_ordinal = ordinal_date_function(end_date,earliest = start_date),
         Nest_location_northing_WGS84_Dec_degree = ifelse(Nest_location_northing_WGS84_Dec_degree < 60,
                                                          63.98658,
                                                          Nest_location_northing_WGS84_Dec_degree),
         Nest_location_Easting_WGS84_Dec_degree = ifelse(Nest_location_Easting_WGS84_Dec_degree > -70,
                                                         -80.76717,
                                                          Nest_location_Easting_WGS84_Dec_degree)) %>%
  filter(Fate %in% c("success", "failed"),
         group == "shorebird",
         !is.na(start_date_ordinal),
         !is.na(end_date_ordinal)) 

clean_nest_fate_data_shore_location <- clean_nest_fate_data_shore %>%
  st_as_sf(coords = c('Nest_location_Easting_WGS84_Dec_degree',
                      'Nest_location_northing_WGS84_Dec_degree')) %>%
  st_set_crs(4326) 

## visualizing the nest locations
## there are still some surprising variation in nest locations
## two clumps of nests that are separated by ~ 1 degree longitude
## and one nest that is south of all others by ~ 1 degree latitude
test_map <- ggplot(data = clean_nest_fate_data_shore_location) +
  geom_sf(aes(color=year))
test_map

## adding snow data

snow_data <- read_csv("raw_data/snowcover_EBM_allyears.csv")
# str(snow_data)

range(snow_data$SnowCover_per, na.rm = TRUE)
#you can't have -1 snow. So the range should be 0-100.
snow_data<- snow_data %>%
  mutate(SnowCover_per = ifelse(SnowCover_per == -1,0,SnowCover_per))
range(snow_data$SnowCover_per, na.rm = TRUE)

# this needs to be the same length as my other covariates
snow_data <- snow_data %>%
  filter(!is.na(SnowCover_per),
         !is.na(obsDate))


#making a for-loop
#create a new column - called SnowCover_per (rename after to something simple)
#if no data, then input 0 

clean_nest_fate_data_shore$snow_per <- 0

for (i in 1:nrow(clean_nest_fate_data_shore)) {
  start_date<- clean_nest_fate_data_shore$start_date[i]
  
  yr <- clean_nest_fate_data_shore$year[i]
  
  if(yr == 1998){
    clean_nest_fate_data_shore$snow_per[i] <- NA
  }else{
    start_week <- start_date+c(-3:3)
    
    tmp_snow <- snow_data %>% 
      filter(obsDate %in% start_week)
    
    snow_fill <- max(tmp_snow$SnowCover_per,na.rm = TRUE)
    
    
    clean_nest_fate_data_shore$snow_per[i] <- ifelse(is.finite(snow_fill),snow_fill,0)
    #break
  }}
range(clean_nest_fate_data_shore$snow_per,na.rm = TRUE)

#I do not want to centre snow because my model should hold snow constant at 0, not at mean snow
#but I do want to scale snow so that the units are comparable
length(which(is.na(clean_nest_fate_data_shore$snow_per)))
clean_nest_fate_data_shore$snow_per_scaled<-scale(clean_nest_fate_data_shore$snow_per, center = FALSE)
range(clean_nest_fate_data_shore$snow_per_scaled) 
mean(clean_nest_fate_data_shore$snow_per_scaled)
sd(clean_nest_fate_data_shore$snow_per_scaled)

### END of the shorebird nests dataset prepare


# generate dataset of gull nests ------------------------------------------
# making into an sf object because that's all it's used for
nest_data_gulls <- east_bay_only_data %>% 
  mutate(start_date = lubridate::dmy(Mayfield_start_date..formula.),
         end_date = lubridate::dmy(Mayfield_end.date..formula.)) %>% 
  filter(group == "gull",
         !is.na(start_date),
         !is.na(end_date)) %>% 
  st_as_sf(coords = c('Nest_location_Easting_WGS84_Dec_degree',
                      'Nest_location_northing_WGS84_Dec_degree')) %>%
  st_set_crs(4326) 


# generate dataset of goose nests -----------------------------------------
# making into an sf object because that's all it's used for

nest_data_goose <- east_bay_only_data %>% 
  mutate(start_date = lubridate::dmy(Mayfield_start_date..formula.),
         end_date = lubridate::dmy(Mayfield_end.date..formula.)) %>% 
  filter(group == "goose",
         !is.na(start_date),
         !is.na(end_date)) %>% 
  st_as_sf(coords = c('Nest_location_Easting_WGS84_Dec_degree',
                      'Nest_location_northing_WGS84_Dec_degree')) %>%
  st_set_crs(4326) 

   

# Loop through each shorebird nest --------------------------------------------------

## this loop fills in the number of shorebird, gull, and goose nests
## that overlap in time and are within set distances of each nest
threshold_distance_shore <- set_units(500, "m")
threshold_distance_gull <- set_units(150, "m")
threshold_distance_goose <- set_units(500, "m")
## the loop works, runs quickly, and personally I like that we know
## that the shorebird nest in question is always the same one
## and so no possibility of a missmatch among matrices


plot_test <- FALSE
## at each stage below (shorebirds, gulls, goose)
## there is a little if{} value that creates a map 
## to visually confirm that the nests identified as neighbours
## are actually that close, so you can set i to a row number to test
## a particular shorebird nest and comfirm that the loop is doing 
## what it's supposed to do.
for(i in 1:nrow(clean_nest_fate_data_shore)){
  
  # nest location
  nest_sf <- clean_nest_fate_data_shore_location[i,]
  # nest dates
  s_date <- clean_nest_fate_data_shore[i,"start_date"]
  e_date <- clean_nest_fate_data_shore[i,"end_date"]
  
  # shorebird nests that overlap in time - removes the nest being assessed
  nest_over <- clean_nest_fate_data_shore_location[-c(i),] %>% 
    filter(start_date < e_date & end_date > s_date)
  
  which_nest_near <- st_is_within_distance(x = nest_sf,
                         y = nest_over,
                         dist = threshold_distance_shore,
                         sparse = FALSE)
  clean_nest_fate_data_shore[i,"n_shore_nests_near"] <- sum(which_nest_near)
  
  if(plot_test){
    
  nests_near <- nest_over[which_nest_near,]
  
  
   test <- ggplot()+
    geom_sf(data = nest_sf,colour = "red")+
    geom_sf(data = nest_over,
            alpha = 0.2)+
    geom_sf(data = nests_near,
            colour = "blue")
  test
  
  }
  
  # gull nests that overlap in time
  gull_nest_over <- nest_data_gulls %>% 
    filter(start_date < e_date & end_date > s_date)
  
  which_gull_nest_near <- st_is_within_distance(x = nest_sf,
                                           y = gull_nest_over,
                                           dist = threshold_distance_gull,
                                           sparse = FALSE)
  clean_nest_fate_data_shore[i,"n_gull_nests_near"] <- sum(which_gull_nest_near)
  
  if(plot_test){
    
    nests_near <- gull_nest_over[which_gull_nest_near,]
    
  test <- ggplot()+
    geom_sf(data = nest_sf,colour = "red")+
    geom_sf(data = gull_nest_over,
            alpha = 0.2)+
    geom_sf(data = nests_near,
            colour = "blue")
  test
  
  }
  # goose nests that overlap in time
  goose_nest_over <- nest_data_goose %>% 
    filter(start_date < e_date & end_date > s_date)
  
  which_goose_nest_near <- st_is_within_distance(x = nest_sf,
                                                y = goose_nest_over,
                                                dist = threshold_distance_goose,
                                                sparse = FALSE)
  clean_nest_fate_data_shore[i,"n_goose_nests_near"] <- sum(which_goose_nest_near)
  
  if(plot_test){
    
    nests_near <- goose_nest_over[which_goose_nest_near,]
    
    test <- ggplot()+
      geom_sf(data = nest_sf,colour = "red")+
      geom_sf(data = goose_nest_over,
              alpha = 0.2)+
      geom_sf(data = nests_near,
              colour = "blue")
    test
    
  }
  
}
      

# with these threshold distances, these values are mostly 0
hist(clean_nest_fate_data_shore$n_goose_nests_near)
hist(clean_nest_fate_data_shore$n_gull_nests_near)
hist(clean_nest_fate_data_shore$n_shore_nests_near)




maxage <- max(clean_nest_fate_data_shore$end_date_ordinal)
nNests <- nrow(clean_nest_fate_data_shore)
## now we have all of the information necessary to build the main data matrix
## of the y[nNests,maxday]

y <- matrix(data = 0,
            nrow = nNests,
            ncol = maxage) # pre-filling the entire matrix with 0 values
## here's a nested loop to fill in the 1-values in the y matrix (there are more concise ways of doing this, but
## sometimes it's nice to make the code super clear)
for(i in 1:nNests){
  first = clean_nest_fate_data_shore[i,"start_date_ordinal"]
  last = clean_nest_fate_data_shore[i,"end_date_ordinal"]
  
  for(j in first : (last-1)){
    y[i,j] <- 1 # setting values to 1 for each nest on the days between first and last
  }
}


#predator indices 
#foxes (sightings) and lemmings becuase the population of lemmings can effect what the foxes predate (they prefer lemmings if available)
species_log<-read.csv("raw_data/specieslog_EBM_allyears.csv")
obs_hours <- read.csv("raw_data/specieslog_observer_Effort_EBM_allyears.csv")
#filter out the "Number of Observers" column and the year 2020 (no data because of pandemic)
filtered_obs_hours <- obs_hours %>%
  filter(log_type != "Number of Observers",
         obsYear != 2020)


#filter for just arctic foxes
species_log_fox <- subset(species_log, species_code== "arctic fox")
#the species log goes back to 1997, but the obs hours only goes back to 2004, also 2020 has no data (because of the pandemic)
filtered_species_log_fox <- species_log_fox %>%
  filter(obsYear >= 2004 & obsYear != 2020)

#sum of foxes per year
fox_sums <- rowSums(filtered_species_log_fox [, 5:72], na.rm = TRUE)
filtered_species_log_fox$total_sum<-fox_sums
#I calculated by hand a few rows by hand and yes, it worked.

#sum of effort hours per year
hours_sums <- rowSums(filtered_obs_hours[, 4:71], na.rm = TRUE)
filtered_obs_hours$total_sum<-hours_sums
#Yes, row 1 is correct

#now divide by the sum of foxes per row by the sum of obs hours per row
#I multiply by 8 to make it number of foxes per 8 hours -- following indices from lit
#I don't think this matters though.
fox_indice <- (filtered_species_log_fox$total_sum/filtered_obs_hours$total_sum)*8
filtered_species_log_fox$fox_indice <- fox_indice

# Visualize this
ggplot(filtered_species_log_fox, aes(x = obsYear, y = fox_indice)) +
  geom_point() +
  geom_line() +
  labs(title = "Scatter Plot of Fox Indice Over Years",
       x = "Year",
       y = "Fox Indice")
#it seems the number of foxes increases over the years. That's interesting.
#also a fairly wide variation between years

#indice for lemmings
#filter for just lemmings
#for the years 2012-2021, they separate brown lemming and green collared lemming
species_log_lemming <- subset(species_log, species_code %in% c("lemming", "brown lemming", "GC lemming"))
#the species log goes back to 1997, but the obs hours only goes back to 2004, also 2020 has no data (because of the pandemic)
filtered_species_log_lemming <- species_log_lemming %>%
  filter(obsYear >= 2004 & obsYear != 2020)

#sum of lemmings per year
lemming_sums <- rowSums(filtered_species_log_lemming [, 5:72], na.rm = TRUE)
filtered_species_log_lemming$total_sum<-lemming_sums
#I calculated by hand a few rows by hand and yes, it worked.
# combine the species of lemmings to make 1 lemmign count per year
lemming_sum_aggregate<-aggregate(filtered_species_log_lemming$total_sum, list(filtered_species_log_lemming$obsYear), FUN = sum)
#This worked. 
#adding this beside the fox indice
lemming_indice <- (lemming_sum_aggregate$x/filtered_obs_hours$total_sum)*8
filtered_species_log_fox$lemming_indice <- lemming_indice
#worked!

# Visualize this
ggplot(filtered_species_log_fox, aes(x = obsYear, y = lemming_indice)) +
  geom_point(aes( y= lemming_indice, colour = "Lemming Indice")) +
  geom_line(aes(y=lemming_indice, colour = "Lemming Indice")) +
  geom_point(aes(y=fox_indice, colour = "Fox Indice")) +
  geom_line(aes(y=fox_indice, colour = "Fox Indice")) +
  labs(title = "Plot of Fox and Lemming Indices Over Years",
       x = "Year",
       y = "Number of sightings per 8 observer hours") +
  scale_color_manual(values = c("Lemming Indice" = "skyblue", "Fox Indice" = "darkorange"))
#cool, they are pretty directly inverted!!


stan_data <- list(Nnests = nrow(clean_nest_fate_data_shore),
                  first_day_as_int_days = clean_nest_fate_data_shore$start_date_ordinal,
                  last_day_as_int_days = clean_nest_fate_data_shore$end_date_ordinal,
                  maxage = maxage,
                  y = y,
                  density_50m = clean_nest_fate_data_shore$n_shore_nests_near,
                  snow_per=as.numeric(clean_nest_fate_data_shore$snow_per_scaled),
                  density_goose = clean_nest_fate_data_shore$n_goose_nests_near,
                  density_gull = clean_nest_fate_data_shore$n_gull_nests_near)


str(stan_data)



#now save this as an .rds
saveRDS(stan_data, "stan_data_list_all.rds")


