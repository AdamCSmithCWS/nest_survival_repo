#This script is to get data for my stan model

setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo")
library(raster) # you may want to switch to using the terra package
# https://rspatial.org/pkg/1-introduction.html
# e.g., https://oceanhealthindex.org/news/raster_to_terra/
# library(dplyr) # dplyr is part of the tidyverse, so you don't need to load it directly
library(tidyverse)
library(sf)
library(RANN)
library(units)

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

#snowmelt -- only applicable at the start of the year. For most of the summer, all snow has melted
#likely positively correlated with density, because more snow = less available nesting habitat
#however, it's at the start of the year only when many birds may not have nested yet.


snow_data <- read_csv("raw_data/snowcover_EBM_allyears.csv")
str(snow_data)

range(snow_data$SnowCover_per, na.rm = TRUE)
#you can't have -1 snow. So the range should be 0-100.
snow_data<- snow_data %>%
  mutate(SnowCover_per = ifelse(SnowCover_per == -1,0,SnowCover_per))
range(snow_data$SnowCover_per, na.rm = TRUE)

# this needs to be the same length as my other covariates
snow_data <- snow_data %>%
  filter(!is.na(SnowCover_per),
         !is.na(obsDate))

clean_nest_fate_data <- clean_nest_fate_data %>%
  filter(!is.na(start_date_ordinal),
         !is.na(end_date_ordinal))


#making a for-loop
#create a new column - called SnowCover_per (rename after to something simple)
#if no data, then input 0 
         
clean_nest_fate_data$snow_per <- 0

for (i in 1:nrow(clean_nest_fate_data)) {
  start_date<- clean_nest_fate_data$start_date[i]
  
  yr <- clean_nest_fate_data$year[i]
  
  if(yr == 1998){
    clean_nest_fate_data$snow_per[i] <- NA
  }else{
    start_week <- start_date+c(-3:3)
    
    tmp_snow <- snow_data %>% 
      filter(obsDate %in% start_week)
    
    snow_fill <- max(tmp_snow$SnowCover_per,na.rm = TRUE)
    
    
    clean_nest_fate_data$snow_per[i] <- ifelse(is.finite(snow_fill),snow_fill,0)
    #break
    }}
range(clean_nest_fate_data$snow_per,na.rm = TRUE)
  
length(which(is.na(clean_nest_fate_data$snow_per)))

#density and nearest neighbour for the year 2000
all_sp_2000<-east_bay_only_data %>%
  filter(year == "2000")
all_sp_2000$Nest_location_northing_WGS84_Dec_degree<-as.numeric(all_sp_2000$Nest_location_northing_WGS84_Dec_degree)
all_sp_2000$Nest_location_Easting_WGS84_Dec_degree<-as.numeric(all_sp_2000$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2000 <- all_sp_2000 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))


#nearest neighbour
nest_loc_2000 <- all_sp_2000 %>%
  st_as_sf(coords = c('Nest_location_Easting_WGS84_Dec_degree',
                      'Nest_location_northing_WGS84_Dec_degree')) %>%
  st_set_crs(4326)

#filter the nests that overlap for at least 1 day
nest_loc_2000$start_date_2000 <- lubridate::dmy(nest_loc_2000$Mayfield_start_date..formula.)
nest_loc_2000$end_date_2000 <- lubridate::dmy(nest_loc_2000$Mayfield_end.date..formula.)
overlapping_nests_2000<- nest_loc_2000 %>%
  mutate(start_date_2000 = ymd(start_date_2000), end_date_2000 = ymd(end_date_2000)) %>%
  filter(start_date_2000 >= (start_date_2000 - days(1)) & end_date_2000 <= (end_date_2000 + days(1)))
#this seems to have worked
#Maybe get Adam to look at this and see if he thinks it worked. 

#this should identify the nearest point for each nest
overlapping_nests_2000$near_neib_2000<-st_nearest_feature(overlapping_nests_2000)
# Create an empty column to store distances
overlapping_nests_2000$dist_nn_2000 <- NA

# Iterate through each nest and calculate the distance to its nearest neighbor
for (i in 1:nrow(overlapping_nests_2000)) {
  nearest_neighbor_index_2000 <- overlapping_nests_2000$near_neib_2000[i]
  distance_2000 <- st_distance(overlapping_nests_2000[i, ], overlapping_nests_2000[nearest_neighbor_index_2000, ])
  overlapping_nests_2000$dist_nn_2000[i] <- distance_2000
}

#filter for only nests that have Fate = Success or Failed
filtered_dist_nn_2000 <- overlapping_nests_2000$dist_nn_2000[which(all_sp_2000$Fate %in% c("success", "failed"))]
View(filtered_dist_nn_2000)
#This worked! filtered_dist_nn_2000 has the distances to the nearest neighbour in meters
# Add nearest neighbor as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(nearest_neigh_dist = ifelse(year == "2000", filtered_dist_nn_2000, NA))



#density within 50m
#create a data matrix of the distances between each nest
#filter the nests that overlap for at least 1 day
dist_2000<-st_as_sf(x=overlapping_nests_2000, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                  "Nest_location_northing_WGS84_Dec_degree"), crs= "WGS84")
distance_matrix_2000<-st_distance(dist_2000)

threshold_distance<-50
threshold_distance<- set_units(50, "m")
exclude_zero<- 0
exclude_zero<- set_units (0, "m")
dens_50m_2000<-numeric (nrow(distance_matrix_2000))


#trying this loop to calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2000)) {
  nest_in_50m_2000 <- sum(distance_matrix_2000[i, ] > exclude_zero & distance_matrix_2000[i, ] < threshold_distance)
  
  dens_50m_2000[i] <- nest_in_50m_2000
}

filtered_dens_2000 <- dens_50m_2000[which(overlapping_nests_2000$Fate %in% c("success", "failed"))]


# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2000", filtered_dens_2000, NA))
#I think this worked!!
#I counted the first four columns (V1,V2,V3, 4) and got 2,1,1,0 respectively!


#to turn into UTM, missing values in coordinates are not allowed
#clean_data_2000 <- clean_data_2000 %>%
 # filter(!is.na(Nest_location_northing_WGS84_Dec_degree),
 #        !is.na(Nest_location_Easting_WGS84_Dec_degree))
#next I need to convert to UTM units
#utm_2000<- st_as_sf(x=clean_data_2000, coords = c("Nest_location_northing_WGS84_Dec_degree","Nest_location_Easting_WGS84_Dec_degree"), crs = 4328)
#coords_2000_utm <- st_transform(utm_2000, crs = "+proj=utm +zone=17")
#st_crs(coords_2000_utm) #I believe I have changed it to EPSG = 32617
#str(coords_2000_utm)




#I need to do this for each year:
#_________________________________________________________________


#density for the year 1998
all_sp_1998 <- east_bay_only_data %>%
  filter(year == "1998")
all_sp_1998$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_1998$Nest_location_northing_WGS84_Dec_degree)
all_sp_1998$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_1998$Nest_location_Easting_WGS84_Dec_degree)
all_sp_1998 <- all_sp_1998 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

#filter the nests that overlap for at least 1 day
all_sp_1998$start_date_1998 <- lubridate::dmy(all_sp_1998$Mayfield_start_date..formula.)
all_sp_1998$end_date_1998 <- lubridate::dmy(all_sp_1998$Mayfield_end.date..formula.)
overlapping_nests_1998<- all_sp_1998 %>%
  mutate(start_date_1998 = ymd(start_date_1998), end_date_2000 = ymd(end_date_1998)) %>%
  filter(start_date_1998 >= (start_date_1998 - days(1)) & end_date_1998 <= (end_date_1998 + days(1)))
# Create a data matrix of the distances between each nest
dist_1998 <- st_as_sf(x = overlapping_nests_1998, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                  "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_1998 <- st_distance(dist_1998)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_1998 <- numeric(nrow(distance_matrix_1998))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_1998)) {
  nest_in_50m_1998 <- sum(distance_matrix_1998[i, ] > exclude_zero & distance_matrix_1998[i, ] < threshold_distance)
  
  dens_50m_1998[i] <- nest_in_50m_1998
}
filtered_dens_1998 <- dens_50m_1998[which(overlapping_nests_1998$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "1998", filtered_dens_1998, density_50m))


# Density for the year 1999
all_sp_1999 <- east_bay_only_data %>%
  filter(year == "1999")
all_sp_1999$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_1999$Nest_location_northing_WGS84_Dec_degree)
all_sp_1999$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_1999$Nest_location_Easting_WGS84_Dec_degree)
all_sp_1999 <- all_sp_1999 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_1999$start_date_1999 <- lubridate::dmy(all_sp_1999$Mayfield_start_date..formula.)
all_sp_1999$end_date_1999 <- lubridate::dmy(all_sp_1999$Mayfield_end.date..formula.)
overlapping_nests_1999 <- all_sp_1999 %>%
  mutate(start_date_1999 = ymd(start_date_1999), end_date_1999 = ymd(end_date_1999)) %>%
  filter(start_date_1999 >= (start_date_1999 - days(1)) & end_date_1999 <= (end_date_1999 + days(1)))

# Create a data matrix of the distances between each nest
dist_1999 <- st_as_sf(x = overlapping_nests_1999, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_1999 <- st_distance(dist_1999)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_1999 <- numeric(nrow(distance_matrix_1999))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_1999)) {
  nest_in_50m_1999 <- sum(distance_matrix_1999[i, ] > exclude_zero & distance_matrix_1999[i, ] < threshold_distance)
  
  dens_50m_1999[i] <- nest_in_50m_1999
}
filtered_dens_1999 <- dens_50m_1999[which(overlapping_nests_1999$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "1999", filtered_dens_1999, density_50m))



# Density for the year 2001
all_sp_2001 <- east_bay_only_data %>%
  filter(year == "2001")
all_sp_2001$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2001$Nest_location_northing_WGS84_Dec_degree)
all_sp_2001$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2001$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2001 <- all_sp_2001 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2001$start_date_2001 <- lubridate::dmy(all_sp_2001$Mayfield_start_date..formula.)
all_sp_2001$end_date_2001 <- lubridate::dmy(all_sp_2001$Mayfield_end.date..formula.)
overlapping_nests_2001 <- all_sp_2001 %>%
  mutate(start_date_2001 = ymd(start_date_2001), end_date_2001 = ymd(end_date_2001)) %>%
  filter(start_date_2001 >= (start_date_2001 - days(1)) & end_date_2001 <= (end_date_2001 + days(1)))

# Create a data matrix of the distances between each nest
dist_2001 <- st_as_sf(x = overlapping_nests_2001, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2001 <- st_distance(dist_2001)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2001 <- numeric(nrow(distance_matrix_2001))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2001)) {
  nest_in_50m_2001 <- sum(distance_matrix_2001[i, ] > exclude_zero & distance_matrix_2001[i, ] < threshold_distance)
  
  dens_50m_2001[i] <- nest_in_50m_2001
}
filtered_dens_2001 <- dens_50m_2001[which(overlapping_nests_2001$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2001", filtered_dens_2001, density_50m))





# Density for the year 2002
all_sp_2002 <- east_bay_only_data %>%
  filter(year == "2002")
all_sp_2002$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2002$Nest_location_northing_WGS84_Dec_degree)
all_sp_2002$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2002$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2002 <- all_sp_2002 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2002$start_date_2002 <- lubridate::dmy(all_sp_2002$Mayfield_start_date..formula.)
all_sp_2002$end_date_2002 <- lubridate::dmy(all_sp_2002$Mayfield_end.date..formula.)
overlapping_nests_2002 <- all_sp_2002 %>%
  mutate(start_date_2002 = ymd(start_date_2002), end_date_2002 = ymd(end_date_2002)) %>%
  filter(start_date_2002 >= (start_date_2002 - days(1)) & end_date_2002 <= (end_date_2002 + days(1)))

# Create a data matrix of the distances between each nest
dist_2002 <- st_as_sf(x = overlapping_nests_2002, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2002 <- st_distance(dist_2002)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2002 <- numeric(nrow(distance_matrix_2002))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2002)) {
  nest_in_50m_2002 <- sum(distance_matrix_2002[i, ] > exclude_zero & distance_matrix_2002[i, ] < threshold_distance)
  
  dens_50m_2002[i] <- nest_in_50m_2002
}
filtered_dens_2002 <- dens_50m_2002[which(overlapping_nests_2002$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2002", filtered_dens_2002, density_50m))




# Density for the year 2003
all_sp_2003 <- east_bay_only_data %>%
  filter(year == "2003")
all_sp_2003$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2003$Nest_location_northing_WGS84_Dec_degree)
all_sp_2003$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2003$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2003 <- all_sp_2003 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2003$start_date_2003 <- lubridate::dmy(all_sp_2003$Mayfield_start_date..formula.)
all_sp_2003$end_date_2003 <- lubridate::dmy(all_sp_2003$Mayfield_end.date..formula.)
overlapping_nests_2003 <- all_sp_2003 %>%
  mutate(start_date_2003 = ymd(start_date_2003), end_date_2003 = ymd(end_date_2003)) %>%
  filter(start_date_2003 >= (start_date_2003 - days(1)) & end_date_2003 <= (end_date_2003 + days(1)))

# Create a data matrix of the distances between each nest
dist_2003 <- st_as_sf(x = overlapping_nests_2003, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2003 <- st_distance(dist_2003)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2003 <- numeric(nrow(distance_matrix_2003))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2003)) {
  nest_in_50m_2003 <- sum(distance_matrix_2003[i, ] > exclude_zero & distance_matrix_2003[i, ] < threshold_distance)
  
  dens_50m_2003[i] <- nest_in_50m_2003
}
filtered_dens_2003 <- dens_50m_2003[which(overlapping_nests_2003$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2003", filtered_dens_2003, density_50m))




# Density for the year 2004
all_sp_2004 <- east_bay_only_data %>%
  filter(year == "2004")
all_sp_2004$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2004$Nest_location_northing_WGS84_Dec_degree)
all_sp_2004$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2004$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2004 <- all_sp_2004 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2004$start_date_2004 <- lubridate::dmy(all_sp_2004$Mayfield_start_date..formula.)
all_sp_2004$end_date_2004 <- lubridate::dmy(all_sp_2004$Mayfield_end.date..formula.)
overlapping_nests_2004 <- all_sp_2004 %>%
  mutate(start_date_2004 = ymd(start_date_2004), end_date_2004 = ymd(end_date_2004)) %>%
  filter(start_date_2004 >= (start_date_2004 - days(1)) & end_date_2004 <= (end_date_2004 + days(1)))

# Create a data matrix of the distances between each nest
dist_2004 <- st_as_sf(x = overlapping_nests_2004, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2004 <- st_distance(dist_2004)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2004 <- numeric(nrow(distance_matrix_2004))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2004)) {
  nest_in_50m_2004 <- sum(distance_matrix_2004[i, ] > exclude_zero & distance_matrix_2004[i, ] < threshold_distance)
  
  dens_50m_2004[i] <- nest_in_50m_2004
}
filtered_dens_2004 <- dens_50m_2004[which(overlapping_nests_2004$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2004", filtered_dens_2004, density_50m))




# Density for the year 2005
all_sp_2005 <- east_bay_only_data %>%
  filter(year == "2005")
all_sp_2005$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2005$Nest_location_northing_WGS84_Dec_degree)
all_sp_2005$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2005$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2005 <- all_sp_2005 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2005$start_date_2005 <- lubridate::dmy(all_sp_2005$Mayfield_start_date..formula.)
all_sp_2005$end_date_2005 <- lubridate::dmy(all_sp_2005$Mayfield_end.date..formula.)
overlapping_nests_2005 <- all_sp_2005 %>%
  mutate(start_date_2005 = ymd(start_date_2005), end_date_2005 = ymd(end_date_2005)) %>%
  filter(start_date_2005 >= (start_date_2005 - days(1)) & end_date_2005 <= (end_date_2005 + days(1)))

# Create a data matrix of the distances between each nest
dist_2005 <- st_as_sf(x = overlapping_nests_2005, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2005 <- st_distance(dist_2005)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2005 <- numeric(nrow(distance_matrix_2005))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2005)) {
  nest_in_50m_2005 <- sum(distance_matrix_2005[i, ] > exclude_zero & distance_matrix_2005[i, ] < threshold_distance)
  
  dens_50m_2005[i] <- nest_in_50m_2005
}
filtered_dens_2005 <- dens_50m_2005[which(overlapping_nests_2005$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2005", filtered_dens_2005, density_50m))




# Density for the year 2006
all_sp_2006 <- east_bay_only_data %>%
  filter(year == "2006")
all_sp_2006$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2006$Nest_location_northing_WGS84_Dec_degree)
all_sp_2006$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2006$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2006 <- all_sp_2006 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2006$start_date_2006 <- lubridate::dmy(all_sp_2006$Mayfield_start_date..formula.)
all_sp_2006$end_date_2006 <- lubridate::dmy(all_sp_2006$Mayfield_end.date..formula.)
overlapping_nests_2006 <- all_sp_2006 %>%
  mutate(start_date_2006 = ymd(start_date_2006), end_date_2006 = ymd(end_date_2006)) %>%
  filter(start_date_2006 >= (start_date_2006 - days(1)) & end_date_2006 <= (end_date_2006 + days(1)))

# Create a data matrix of the distances between each nest
dist_2006 <- st_as_sf(x = overlapping_nests_2006, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2006 <- st_distance(dist_2006)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2006 <- numeric(nrow(distance_matrix_2006))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2006)) {
  nest_in_50m_2006 <- sum(distance_matrix_2006[i, ] > exclude_zero & distance_matrix_2006[i, ] < threshold_distance)
  
  dens_50m_2006[i] <- nest_in_50m_2006
}
filtered_dens_2006 <- dens_50m_2006[which(overlapping_nests_2006$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2006", filtered_dens_2006, density_50m))




# Density for the year 2007
all_sp_2007 <- east_bay_only_data %>%
  filter(year == "2007")
all_sp_2007$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2007$Nest_location_northing_WGS84_Dec_degree)
all_sp_2007$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2007$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2007 <- all_sp_2007 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2007$start_date_2007 <- lubridate::dmy(all_sp_2007$Mayfield_start_date..formula.)
all_sp_2007$end_date_2007 <- lubridate::dmy(all_sp_2007$Mayfield_end.date..formula.)
overlapping_nests_2007 <- all_sp_2007 %>%
  mutate(start_date_2007 = ymd(start_date_2007), end_date_2007 = ymd(end_date_2007)) %>%
  filter(start_date_2007 >= (start_date_2007 - days(1)) & end_date_2007 <= (end_date_2007 + days(1)))

# Create a data matrix of the distances between each nest
dist_2007 <- st_as_sf(x = overlapping_nests_2007, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2007 <- st_distance(dist_2007)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2007 <- numeric(nrow(distance_matrix_2007))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2007)) {
  nest_in_50m_2007 <- sum(distance_matrix_2007[i, ] > exclude_zero & distance_matrix_2007[i, ] < threshold_distance)
  
  dens_50m_2007[i] <- nest_in_50m_2007
}
filtered_dens_2007 <- dens_50m_2007[which(overlapping_nests_2007$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2007", filtered_dens_2007, density_50m))




# Density for the year 2008
all_sp_2008 <- east_bay_only_data %>%
  filter(year == "2008")
all_sp_2008$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2008$Nest_location_northing_WGS84_Dec_degree)
all_sp_2008$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2008$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2008 <- all_sp_2008 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2008$start_date_2008 <- lubridate::dmy(all_sp_2008$Mayfield_start_date..formula.)
all_sp_2008$end_date_2008 <- lubridate::dmy(all_sp_2008$Mayfield_end.date..formula.)
overlapping_nests_2008 <- all_sp_2008 %>%
  mutate(start_date_2008 = ymd(start_date_2008), end_date_2008 = ymd(end_date_2008)) %>%
  filter(start_date_2008 >= (start_date_2008 - days(1)) & end_date_2008 <= (end_date_2008 + days(1)))

# Create a data matrix of the distances between each nest
dist_2008 <- st_as_sf(x = overlapping_nests_2008, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2008 <- st_distance(dist_2008)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2008 <- numeric(nrow(distance_matrix_2008))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2008)) {
  nest_in_50m_2008 <- sum(distance_matrix_2008[i, ] > exclude_zero & distance_matrix_2008[i, ] < threshold_distance)
  
  dens_50m_2008[i] <- nest_in_50m_2008
}
filtered_dens_2008 <- dens_50m_2008[which(overlapping_nests_2008$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2008", filtered_dens_2008, density_50m))




# Density for the year 2009
all_sp_2009 <- east_bay_only_data %>%
  filter(year == "2009")
all_sp_2009$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2009$Nest_location_northing_WGS84_Dec_degree)
all_sp_2009$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2009$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2009 <- all_sp_2009 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2009$start_date_2009 <- lubridate::dmy(all_sp_2009$Mayfield_start_date..formula.)
all_sp_2009$end_date_2009 <- lubridate::dmy(all_sp_2009$Mayfield_end.date..formula.)
overlapping_nests_2009 <- all_sp_2009 %>%
  mutate(start_date_2009 = ymd(start_date_2009), end_date_2009 = ymd(end_date_2009)) %>%
  filter(start_date_2009 >= (start_date_2009 - days(1)) & end_date_2009 <= (end_date_2009 + days(1)))

# Create a data matrix of the distances between each nest
dist_2009 <- st_as_sf(x = overlapping_nests_2009, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2009 <- st_distance(dist_2009)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2009 <- numeric(nrow(distance_matrix_2009))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2009)) {
  nest_in_50m_2009 <- sum(distance_matrix_2009[i, ] > exclude_zero & distance_matrix_2009[i, ] < threshold_distance)
  
  dens_50m_2009[i] <- nest_in_50m_2009
}
filtered_dens_2009 <- dens_50m_2009[which(overlapping_nests_2009$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2009", filtered_dens_2009, density_50m))





# Density for the year 2010
all_sp_2010 <- east_bay_only_data %>%
  filter(year == "2010")
all_sp_2010$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2010$Nest_location_northing_WGS84_Dec_degree)
all_sp_2010$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2010$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2010 <- all_sp_2010 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2010$start_date_2010 <- lubridate::dmy(all_sp_2010$Mayfield_start_date..formula.)
all_sp_2010$end_date_2010 <- lubridate::dmy(all_sp_2010$Mayfield_end.date..formula.)
overlapping_nests_2010 <- all_sp_2010 %>%
  mutate(start_date_2010 = ymd(start_date_2010), end_date_2010 = ymd(end_date_2010)) %>%
  filter(start_date_2010 >= (start_date_2010 - days(1)) & end_date_2010 <= (end_date_2010 + days(1)))

# Create a data matrix of the distances between each nest
dist_2010 <- st_as_sf(x = overlapping_nests_2010, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2010 <- st_distance(dist_2010)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2010 <- numeric(nrow(distance_matrix_2010))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2010)) {
  nest_in_50m_2010 <- sum(distance_matrix_2010[i, ] > exclude_zero & distance_matrix_2010[i, ] < threshold_distance)
  
  dens_50m_2010[i] <- nest_in_50m_2010
}
filtered_dens_2010 <- dens_50m_2010[which(overlapping_nests_2010$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2010", filtered_dens_2010, density_50m))




# Density for the year 2011
all_sp_2011 <- east_bay_only_data %>%
  filter(year == "2011")
all_sp_2011$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2011$Nest_location_northing_WGS84_Dec_degree)
all_sp_2011$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2011$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2011 <- all_sp_2011 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2011$start_date_2011 <- lubridate::dmy(all_sp_2011$Mayfield_start_date..formula.)
all_sp_2011$end_date_2011 <- lubridate::dmy(all_sp_2011$Mayfield_end.date..formula.)
overlapping_nests_2011 <- all_sp_2011 %>%
  mutate(start_date_2011 = ymd(start_date_2011), end_date_2011 = ymd(end_date_2011)) %>%
  filter(start_date_2011 >= (start_date_2011 - days(1)) & end_date_2011 <= (end_date_2011 + days(1)))

# Create a data matrix of the distances between each nest
dist_2011 <- st_as_sf(x = overlapping_nests_2011, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2011 <- st_distance(dist_2011)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2011 <- numeric(nrow(distance_matrix_2011))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2011)) {
  nest_in_50m_2011 <- sum(distance_matrix_2011[i, ] > exclude_zero & distance_matrix_2011[i, ] < threshold_distance)
  
  dens_50m_2011[i] <- nest_in_50m_2011
}
filtered_dens_2011 <- dens_50m_2011[which(overlapping_nests_2011$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2011", filtered_dens_2011, density_50m))




                      
# Density for the year 2012
all_sp_2012 <- east_bay_only_data %>%
  filter(year == "2012")
all_sp_2012$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2012$Nest_location_northing_WGS84_Dec_degree)
all_sp_2012$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2012$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2012 <- all_sp_2012 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2012$start_date_2012 <- lubridate::dmy(all_sp_2012$Mayfield_start_date..formula.)
all_sp_2012$end_date_2012 <- lubridate::dmy(all_sp_2012$Mayfield_end.date..formula.)
overlapping_nests_2012 <- all_sp_2012 %>%
  mutate(start_date_2012 = ymd(start_date_2012), end_date_2012 = ymd(end_date_2012)) %>%
  filter(start_date_2012 >= (start_date_2012 - days(1)) & end_date_2012 <= (end_date_2012 + days(1)))

# Create a data matrix of the distances between each nest
dist_2012 <- st_as_sf(x = overlapping_nests_2012, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2012 <- st_distance(dist_2012)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2012 <- numeric(nrow(distance_matrix_2012))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2012)) {
  nest_in_50m_2012 <- sum(distance_matrix_2012[i, ] > exclude_zero & distance_matrix_2012[i, ] < threshold_distance)
  
  dens_50m_2012[i] <- nest_in_50m_2012
}
filtered_dens_2012 <- dens_50m_2012[which(overlapping_nests_2012$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2012", filtered_dens_2012, density_50m))





# Density for the year 2013
all_sp_2013 <- east_bay_only_data %>%
  filter(year == "2013")
all_sp_2013$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2013$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2013$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2013$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2013 <- all_sp_2013 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2013$start_date_2013 <- lubridate::dmy(all_sp_2013$Mayfield_start_date_formula)
all_sp_2013$end_date_2013 <- lubridate::dmy(all_sp_2013$Mayfield_end_date_formula)
overlapping_nests_2013 <- all_sp_2013 %>%
  mutate(start_date_2013 = ymd(start_date_2013), end_date_2013 = ymd(end_date_2013)) %>%
  filter(start_date_2013 >= (start_date_2013 - days(1)) & end_date_2013 <= (end_date_2013 + days(1)))

# Create a data matrix of the distances between each nest
dist_2013 <- st_as_sf(x = overlapping_nests_2013, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2013 <- st_distance(dist_2013)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2013 <- numeric(nrow(distance_matrix_2013))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2013)) {
  nest_in_50m_2013 <- sum(distance_matrix_2013[i, ] > exclude_zero & distance_matrix_2013[i, ] < threshold_distance)
  
  dens_50m_2013[i] <- nest_in_50m_2013
}
filtered_dens_2013 <- dens_50m_2013[which(overlapping_nests_2013$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2013", filtered_dens_2013, density_50m))





# Density for the year 2014
all_sp_2014 <- east_bay_only_data %>%
  filter(year == "2014")
all_sp_2014$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2014$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2014$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2014$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2014 <- all_sp_2014 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2014$start_date_2014 <- lubridate::dmy(all_sp_2014$Mayfield_start_date_formula)
all_sp_2014$end_date_2014 <- lubridate::dmy(all_sp_2014$Mayfield_end_date_formula)
overlapping_nests_2014 <- all_sp_2014 %>%
  mutate(start_date_2014 = ymd(start_date_2014), end_date_2014 = ymd(end_date_2014)) %>%
  filter(start_date_2014 >= (start_date_2014 - days(1)) & end_date_2014 <= (end_date_2014 + days(1)))

# Create a data matrix of the distances between each nest
dist_2014 <- st_as_sf(x = overlapping_nests_2014, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2014 <- st_distance(dist_2014)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2014 <- numeric(nrow(distance_matrix_2014))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2014)) {
  nest_in_50m_2014 <- sum(distance_matrix_2014[i, ] > exclude_zero & distance_matrix_2014[i, ] < threshold_distance)
  
  dens_50m_2014[i] <- nest_in_50m_2014
}
filtered_dens_2014 <- dens_50m_2014[which(overlapping_nests_2014$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2014", filtered_dens_2014, density_50m))





# Density for the year 2015
all_sp_2015 <- east_bay_only_data %>%
  filter(year == "2015")
all_sp_2015$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2015$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2015$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2015$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2015 <- all_sp_2015 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2015$start_date_2015 <- lubridate::dmy(all_sp_2015$Mayfield_start_date_formula)
all_sp_2015$end_date_2015 <- lubridate::dmy(all_sp_2015$Mayfield_end_date_formula)
overlapping_nests_2015 <- all_sp_2015 %>%
  mutate(start_date_2015 = ymd(start_date_2015), end_date_2015 = ymd(end_date_2015)) %>%
  filter(start_date_2015 >= (start_date_2015 - days(1)) & end_date_2015 <= (end_date_2015 + days(1)))

# Create a data matrix of the distances between each nest
dist_2015 <- st_as_sf(x = overlapping_nests_2015, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2015 <- st_distance(dist_2015)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2015 <- numeric(nrow(distance_matrix_2015))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2015)) {
  nest_in_50m_2015 <- sum(distance_matrix_2015[i, ] > exclude_zero & distance_matrix_2015[i, ] < threshold_distance)
  
  dens_50m_2015[i] <- nest_in_50m_2015
}
filtered_dens_2015 <- dens_50m_2015[which(overlapping_nests_2015$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2015", filtered_dens_2015, density_50m))





# Density for the year 2016
all_sp_2016 <- east_bay_only_data %>%
  filter(year == "2016")
all_sp_2016$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2016$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2016$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2016$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2016 <- all_sp_2016 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2016$start_date_2016 <- lubridate::dmy(all_sp_2016$Mayfield_start_date_formula)
all_sp_2016$end_date_2016 <- lubridate::dmy(all_sp_2016$Mayfield_end_date_formula)
overlapping_nests_2016 <- all_sp_2016 %>%
  mutate(start_date_2016 = ymd(start_date_2016), end_date_2016 = ymd(end_date_2016)) %>%
  filter(start_date_2016 >= (start_date_2016 - days(1)) & end_date_2016 <= (end_date_2016 + days(1)))

# Create a data matrix of the distances between each nest
dist_2016 <- st_as_sf(x = overlapping_nests_2016, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2016 <- st_distance(dist_2016)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2016 <- numeric(nrow(distance_matrix_2016))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2016)) {
  nest_in_50m_2016 <- sum(distance_matrix_2016[i, ] > exclude_zero & distance_matrix_2016[i, ] < threshold_distance)
  
  dens_50m_2016[i] <- nest_in_50m_2016
}
filtered_dens_2016 <- dens_50m_2016[which(overlapping_nests_2016$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2016", filtered_dens_2016, density_50m))




# Density for the year 2017
all_sp_2017 <- east_bay_only_data %>%
  filter(year == "2017")
all_sp_2017$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2017$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2017$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2017$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2017 <- all_sp_2017 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2017$start_date_2017 <- lubridate::dmy(all_sp_2017$Mayfield_start_date_formula)
all_sp_2017$end_date_2017 <- lubridate::dmy(all_sp_2017$Mayfield_end_date_formula)
overlapping_nests_2017 <- all_sp_2017 %>%
  mutate(start_date_2017 = ymd(start_date_2017), end_date_2017 = ymd(end_date_2017)) %>%
  filter(start_date_2017 >= (start_date_2017 - days(1)) & end_date_2017 <= (end_date_2017 + days(1)))

# Create a data matrix of the distances between each nest
dist_2017 <- st_as_sf(x = overlapping_nests_2017, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2017 <- st_distance(dist_2017)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2017 <- numeric(nrow(distance_matrix_2017))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2017)) {
  nest_in_50m_2017 <- sum(distance_matrix_2017[i, ] > exclude_zero & distance_matrix_2017[i, ] < threshold_distance)
  
  dens_50m_2017[i] <- nest_in_50m_2017
}
filtered_dens_2017 <- dens_50m_2017[which(overlapping_nests_2017$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2017", filtered_dens_2017, density_50m))




# Density for the year 2018
all_sp_2018 <- east_bay_only_data %>%
  filter(year == "2018")
all_sp_2018$Nest_location_northing_WGS84_Dec_degree <- as.numeric(all_sp_2018$Nest_location_Northing_WGS84_Dec_degree)
all_sp_2018$Nest_location_Easting_WGS84_Dec_degree <- as.numeric(all_sp_2018$Nest_location_Easting_WGS84_Dec_degree)
all_sp_2018 <- all_sp_2018 %>%
  filter(!is.na(Nest_location_Easting_WGS84_Dec_degree),
         !is.na(Nest_location_northing_WGS84_Dec_degree))

# Filter the nests that overlap for at least 1 day
all_sp_2018$start_date_2018 <- lubridate::dmy(all_sp_2018$Mayfield_start_date_formula)
all_sp_2018$end_date_2018 <- lubridate::dmy(all_sp_2018$Mayfield_end_date_formula)
overlapping_nests_2018 <- all_sp_2018 %>%
  mutate(start_date_2018 = ymd(start_date_2018), end_date_2018 = ymd(end_date_2018)) %>%
  filter(start_date_2018 >= (start_date_2018 - days(1)) & end_date_2018 <= (end_date_2018 + days(1)))

# Create a data matrix of the distances between each nest
dist_2018 <- st_as_sf(x = overlapping_nests_2018, coords = c("Nest_location_Easting_WGS84_Dec_degree",
                                                             "Nest_location_northing_WGS84_Dec_degree"), crs = "WGS84")
distance_matrix_2018 <- st_distance(dist_2018)

threshold_distance <- 50
threshold_distance <- set_units(50, "m")
exclude_zero <- 0
exclude_zero <- set_units(0, "m")
dens_50m_2018 <- numeric(nrow(distance_matrix_2018))

# Calculate the number of nests within 50m, but not including 0 (itself)
for (i in 1:nrow(distance_matrix_2018)) {
  nest_in_50m_2018 <- sum(distance_matrix_2018[i, ] > exclude_zero & distance_matrix_2018[i, ] < threshold_distance)
  
  dens_50m_2018[i] <- nest_in_50m_2018
}
filtered_dens_2018 <- dens_50m_2018[which(overlapping_nests_2018$Fate %in% c("success", "failed"))]

# Add the density as a new column to the original dataframe
clean_nest_fate_data <- clean_nest_fate_data %>%
  mutate(density_50m = ifelse(year == "2018", filtered_dens_2018, density_50m))

#______________________________________________________________________________

#let's try binding these utm coordinates together
#utm_coords_list<- list(coords_2000_utm, coords_2001_utm, coords_2002_utm,
                          #coords_2003_utm, coords_2004_utm, coords_2005_utm)
#coords_utm_5years <- do.call(rbind, utm_coords_list)

#determining number of nests within 50m
#adapted from Freeman et al., 2023
#first for year 2000

#find the minimum distance excluding itself (zero) -- so this is nearest neighbour



### removing nests with NA values in the start and end dates and the eggs_hatched column
clean_nest_fate_data <- clean_nest_fate_data %>%
  filter(!is.na(start_date_ordinal),
         !is.na(end_date_ordinal),
         !is.na(density_50m),
         !is.na(snow_per))

range(clean_nest_fate_data$density_50m)
range(clean_nest_fate_data$start_date_ordinal)
range(clean_nest_fate_data$end_date_ordinal)
range(clean_nest_fate_data$snow_per)
# [1]  14 128 ## the 128 is because of an error in the end date -- apparently the nest end_date is october 6, 1952!
hist(clean_nest_fate_data$end_date_ordinal)
## other than that one value all others are less than 60

#clean_nest_fate_data <- clean_nest_fate_data %>%
 # filter(end_date_ordinal < 60) %>%
  #mutate(eggs_hatched_scaled = as.numeric(scale(eggs_hatched))) # scaling the predictor (mean = 0, sd = 1)
#DO I WANT TO SCALE MY DENSITY AND SNOW_PER VARIABLE??

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
#print(mismatched_data)
#View(mismatched_data)
# Subset ndays_df and ndays_matrix where differences are not equal to 0
mismatched_ndays_df <- ndays_df[differences]
mismatched_ndays_matrix <- ndays_matrix[differences]

# Create a data frame to display the differences
mismatched_values <- data.frame(
  ndays_df = mismatched_ndays_df,
  ndays_matrix = mismatched_ndays_matrix
)
# Display the data frame with the mismatched values
#View(mismatched_values)


# making the Stan data list -----------------------------------------------


## Stan requires a named list as a data object

  stan_data <- list(Nnests = nNests,
                    first_day_as_int_days = clean_nest_fate_data$start_date_ordinal,
                    last_day_as_int_days = clean_nest_fate_data$end_date_ordinal,
                    maxage = maxage,
                    y = y,
                    density_50m = clean_nest_fate_data$density_50m,
                    snow_per=clean_nest_fate_data$snow_per)



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

