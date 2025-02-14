


library(tidyverse)
library(sf)
library(units)
library(ggplot2)
library(readxl)


## generate full dataset of shorebird nests with known fate
all_data<-read_excel("raw_data/EBM Nest Locations_2016-2024.xlsx")

# Full model
# 
# Response:
#   Nest survival
# 
# Predictors: 
#   Nest density * Fox abundance
# 
# Controls:
#   Lemming abundance
# Goose density
# Snow cover
# 
# 
# 
# 

#first_day_field_season <- min(yday(all_data$mayfield_start_date),na.rm = T)
## code was used to count the number of nests for each species
# sp_w_data <- all_data %>% 
#   group_by(species) %>% 
#   summarise(n_nests = n())
# write_csv(sp_w_data,"species_list.csv")
## then this csv file was edited manually to add a 
## species group column that allows for selecting shorebirds
## gulls etc.
sp_list <- read_csv("species_list.csv") %>% 
  select(-n_nests) %>% 
  rename(species_code = species)

all_data <- all_data %>% 
  inner_join(sp_list)





# Creating full dataset of shorebird nests --------------------------------



## function to calculate ordinal day of the 
## season, 
# ordinal_date_function <- function(x, earliest = NULL){
#   oday <- lubridate::yday(lubridate::as_date(x))
#   if(is.null(earliest)){
#     earliest <- min(oday, na.rm = TRUE) -1
#   }else{
#     earliest <- lubridate::yday(lubridate::as_date(earliest))
#     earliest <- min(earliest,na.rm = TRUE)-1
#   }
#   oday <- oday - earliest
#   return(oday)
# }

clean_nest_fate_data_shore <- all_data %>%
  filter(nest_fate %in% c("success", "failed"),
    group == "shorebird") %>% 
  mutate(start_date_doy = yday(mayfield_start_date),
         end_date_doy = yday(mayfield_end_date))  %>%
  filter(!is.na(start_date_doy),
    !is.na(end_date_doy))




clean_nest_fate_data_shore_location <- clean_nest_fate_data_shore %>%
  st_as_sf(coords = c('longitude_dd',
                      'latitude_dd')) %>%
  st_set_crs(4326) 

## visualizing the nest locations
## there are still some surprising variation in nest locations
## two clumps of nests that are separated by ~ 1 degree longitude
## and one nest that is south of all others by ~ 1 degree latitude
test_map <- ggplot(data = clean_nest_fate_data_shore_location) +
  geom_sf(aes(color=species_code))
test_map

## adding snow data

snow_data_raw <- read_xlsx("raw_data/EBM Snow Cover_2016-2024.xlsx")
# str(snow_data)

range(snow_data_raw$SnowCover_per, na.rm = TRUE)



# this needs to be the same length as all covariates
snow_data_raw <- snow_data_raw %>%
  filter(!is.na(SnowCover_per),
         !is.na(obsDate)) %>% 
  select(MonitoringSite:SnowCover_per) %>% 
  mutate(doy = yday(obsDate))

snow_data_temp <- snow_data_raw %>% 
  group_by(obsYear) %>% 
  summarise(min_doy = min(doy))

snow_data <- expand_grid(obsYear = unique(snow_data_raw$obsYear),
                         doy = seq(from = min(snow_data_raw$doy), to = max(snow_data_raw$doy)))

# fill in the implied 0 snowcover values on dates after snowcover data were no longer
# collected in each year
snow_data <- snow_data %>% 
  left_join(snow_data_raw,
            by = c("obsYear","doy")) %>% 
  left_join(snow_data_temp,by = "obsYear") %>% 
  filter(doy >= min_doy) %>% 
  mutate(SnowCover_per = ifelse(is.na(SnowCover_per),0,SnowCover_per))


# snow_data is a daily estimate of snowcover for every possible day of 
# each annual field season, including the implied zero values
# It also includes days at teh end of the season where field work may not have been conducted
# but these extra days should be irrelevant because there will be no field data associated 
# with them

for(i in 1:nrow(clean_nest_fate_data_shore)){
  y <- as.integer(clean_nest_fate_data_shore[i,"obsYear"])
  sd <- as.integer(clean_nest_fate_data_shore[i,"start_date_doy"])
  ed <- as.integer(clean_nest_fate_data_shore[i,"end_date_doy"])
  
  sn_dur <- snow_data %>% 
    filter(obsYear == y,
           doy <= ed & doy >= sd)
  
  clean_nest_fate_data_shore[i,"mean_snow_cover"] <- mean(as.numeric(sn_dur$SnowCover_per))
  
  clean_nest_fate_data_shore[i,"p_days_no_snow_cover"] <- length(which(sn_dur$SnowCover_per <= 0))/((ed-sd)+1)
  
  sn_dur <- snow_data %>% 
    filter(obsYear == y,
           doy == sd)
  
  clean_nest_fate_data_shore[i,"start_date_snow_cover"] <- (as.numeric(sn_dur$SnowCover_per))
}


### END of the shorebird nests dataset prepare


# generate dataset of gull nests ------------------------------------------
# making into an sf object because that's all it's used for
clean_nest_fate_data_gull <- all_data %>%
  filter(#Fate %in% c("success", "failed"),
    group == "gull") %>% 
  mutate(start_date_doy = yday(mayfield_start_date),
         end_date_doy = yday(mayfield_end_date))  %>%
  filter(!is.na(start_date_doy),
         !is.na(end_date_doy)) %>% 
  st_as_sf(coords = c('longitude_dd',
                      'latitude_dd')) %>%
  st_set_crs(4326) 


# generate dataset of goose nests -----------------------------------------
# making into an sf object because that's all it's used for

clean_nest_fate_data_goose <- all_data %>%
  filter(#Fate %in% c("success", "failed"),
    group == "goose") %>% 
  mutate(start_date_doy = yday(mayfield_start_date),
         end_date_doy = yday(mayfield_end_date))  %>%
  filter(!is.na(start_date_doy),
         !is.na(end_date_doy)) %>% 
  st_as_sf(coords = c('longitude_dd',
                      'latitude_dd')) %>%
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


plot_test <- TRUE
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
  s_date <- unlist(clean_nest_fate_data_shore[i,"mayfield_start_date"])
  e_date <- unlist(clean_nest_fate_data_shore[i,"mayfield_end_date"])
  
  # shorebird nests that overlap in time - removes the nest being assessed
  nest_over <- clean_nest_fate_data_shore_location[-c(i),] %>% 
    filter(mayfield_start_date <= e_date & mayfield_end_date >= s_date)
  
  which_nest_near <- st_is_within_distance(x = nest_sf,
                         y = nest_over,
                         dist = threshold_distance_shore,
                         sparse = FALSE)
  which_nest_near <- as.logical(which_nest_near)
  clean_nest_fate_data_shore[i,"n_shore_nests_near"] <- sum(which_nest_near)
  
  if(plot_test & sum(which_nest_near) > 0){
    
  nests_near <- nest_over[unlist(which_nest_near),]
  
  
   test <- ggplot()+
    geom_sf(data = nest_sf,colour = "red")+
    geom_sf(data = nest_over,
            alpha = 0.2)+
    geom_sf(data = nests_near,
            colour = "blue")
  test
  
  }
  
  # gull nests that overlap in time
  gull_nest_over <- clean_nest_fate_data_gull %>%
    filter(mayfield_start_date <= e_date & mayfield_end_date >= s_date)
  
  which_gull_nest_near <- st_is_within_distance(x = nest_sf,
                                           y = gull_nest_over,
                                           dist = threshold_distance_gull,
                                           sparse = FALSE)
  which_gull_nest_near <- as.logical(which_gull_nest_near)
  
  clean_nest_fate_data_shore[i,"n_gull_nests_near"] <- sum(which_gull_nest_near)
  
  if(plot_test & sum(which_gull_nest_near) > 0){
    
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
  goose_nest_over <- clean_nest_fate_data_goose %>%
    filter(mayfield_start_date <= e_date & mayfield_end_date >= s_date)
  
  which_goose_nest_near <- st_is_within_distance(x = nest_sf,
                                                y = goose_nest_over,
                                                dist = threshold_distance_goose,
                                                sparse = FALSE)
  which_goose_nest_near <- as.logical(which_goose_nest_near)
  clean_nest_fate_data_shore[i,"n_goose_nests_near"] <- sum(which_goose_nest_near)
  
  if(plot_test & sum(which_goose_nest_near) > 0){
    
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


doy_first_nest_obs <- min(clean_nest_fate_data_shore$start_date_doy)

maxdoy <- max(clean_nest_fate_data_shore$end_date_doy)-(doy_first_nest_obs-1)
nNests <- nrow(clean_nest_fate_data_shore)
## now we have all of the information necessary to build the main data matrix
## of the y[nNests,maxday]

y <- matrix(data = 0,
            nrow = nNests,
            ncol = maxdoy) # pre-filling the entire matrix with 0 values
## here's a nested loop to fill in the 1-values in the y matrix (there are more concise ways of doing this, but
## sometimes it's nice to make the code super clear)
for(i in 1:nNests){
  first = as.integer(clean_nest_fate_data_shore[i,"start_date_doy"])-(doy_first_nest_obs-1)
  last = as.integer(clean_nest_fate_data_shore[i,"end_date_doy"])-(doy_first_nest_obs-1)
  
  for(j in first : (last-1)){
    y[i,j] <- 1 # setting values to 1 for each nest on the days between first and last
  }
}


#predator indices 
#foxes (sightings) and lemmings becuase the population of lemmings can effect what the foxes predate (they prefer lemmings if available)
species_log<-read_xlsx("raw_data/EBM Species Log_2016-2024.xlsx")
obs_hours <- read_xlsx("raw_data/EBM Species Log_effort_2016-2024.xlsx")
#filter out the "Number of Observers" column and the year 2020 (no data because of pandemic)
filtered_obs_hours <- obs_hours %>%
  filter(log_type != "Number of Observers",
         obsYear != 2020)


#filter for just arctic foxes
species_log_fox <- species_log %>% 
  filter(species_code== "arctic fox")


#sum of foxes per year
fox_sums <- rowSums(species_log_fox [, -c(1:4)], na.rm = TRUE)
species_log_fox$total_sum<-fox_sums
#I calculated by hand a few rows by hand and yes, it worked.

#sum of effort hours per year
hours_sums <- rowSums(filtered_obs_hours[,-c(1:3)], na.rm = TRUE)
filtered_obs_hours$total_sum<-hours_sums

#now divide by the sum of foxes per row by the sum of obs hours per row
#I multiply by 8 to make it number of foxes per 8 hours -- following indices from lit
#I don't think this matters though.
fox_indice <- (species_log_fox$total_sum/filtered_obs_hours$total_sum)*8
species_log_fox$fox_indice <- fox_indice

# Visualize this
ggplot(species_log_fox, aes(x = obsYear, y = fox_indice)) +
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
  filter(obsYear != 2020)

#sum of lemmings per year
lemming_sums <- rowSums(filtered_species_log_lemming [,-c(1:4)], na.rm = TRUE)
filtered_species_log_lemming$total_sum<-lemming_sums
#I calculated by hand a few rows by hand and yes, it worked.
# combine the species of lemmings to make 1 lemmign count per year
lemming_sum_aggregate<-aggregate(filtered_species_log_lemming$total_sum, list(filtered_species_log_lemming$obsYear), FUN = sum)
#This worked. 
#adding this beside the fox indice
lemming_indice <- (lemming_sum_aggregate$x/filtered_obs_hours$total_sum)*8
species_log_fox$lemming_indice <- lemming_indice
#worked!

# Visualize this
ggplot(species_log_fox, aes(x = obsYear, y = lemming_indice)) +
  geom_point(aes( y= lemming_indice, colour = "Lemming Indice")) +
  geom_line(aes(y=lemming_indice, colour = "Lemming Indice")) +
  geom_point(aes(y=fox_indice, colour = "Fox Indice")) +
  geom_line(aes(y=fox_indice, colour = "Fox Indice")) +
  labs(title = "Plot of Fox and Lemming Indices Over Years",
       x = "Year",
       y = "Number of sightings per 8 observer hours") +
  scale_color_manual(values = c("Lemming Indice" = "skyblue", "Fox Indice" = "darkorange"))




stan_data <- list(Nnests = nrow(clean_nest_fate_data_shore),
                  first_day_as_int_days = clean_nest_fate_data_shore$start_date_doy-(doy_first_nest_obs-1),
                  last_day_as_int_days = clean_nest_fate_data_shore$end_date_doy-(doy_first_nest_obs-1),
                  maxage = maxdoy,
                  y = y,
                  density_50m = clean_nest_fate_data_shore$n_shore_nests_near,
                  snow_per=as.numeric(clean_nest_fate_data_shore$mean_snow_cover),
                  density_goose = clean_nest_fate_data_shore$n_goose_nests_near,
                  density_gull = clean_nest_fate_data_shore$n_gull_nests_near)


str(stan_data)



#now save this as an .rds
saveRDS(stan_data, "stan_data_list_all.rds")


