


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
  mutate(day_found = yday(date_nest_found),
         day_last_alive = yday(date_last_active),
         day_last_checked = yday(date_last_checked))  %>%
  filter(!is.na(day_last_alive),
    !is.na(day_found),
    !is.na(day_last_checked),
    day_last_alive >= day_found)




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
  s_date <- unlist(clean_nest_fate_data_shore[i,"date_nest_found"])
  e_date <- unlist(clean_nest_fate_data_shore[i,"date_last_checked"])
  
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







# generate an observation dataset for all checks by nest ------------------

obs_data <- clean_nest_fate_data_shore %>% 
  mutate(interval0 = day_found-day_found,
         interval1 = ifelse(day_last_alive-day_found == 0,
                            day_last_checked-day_last_alive,
                            day_last_alive-day_found),
         interval2 = ifelse(day_last_alive-day_found > 0,
                            day_last_checked-day_last_alive,
                            NA)) %>% 
  #filter(interval1 < 10, interval2 < 10) %>% 
  select(obsYear,loc_year_nestID,species_full,species_code,
         latitude_dd,longitude_dd,
         group,nest_fate,fail_cause,
         incubation_duration_for_species,
         day_found,day_last_alive,day_last_checked,
         n_shore_nests_near,n_gull_nests_near,n_goose_nests_near,
         interval0,interval1,interval2) %>% 
  pivot_longer(cols = c(interval0,interval1,interval2),
               values_to = "interval") %>% 
  filter(!(interval == 0 & name %in% c("interval1","interval2"))) %>% 
  mutate(check = ifelse(name == "interval0",0,1),
         active = ifelse(nest_fate == "success" |
                           (name %in% c("interval0","interval1")),1,0),
         int = ifelse(name == "interval0",0,1),
         int = ifelse(name == "interval2",2,int),
         year = as.integer(factor(obsYear))) 

nest_one_check <- obs_data %>% group_by(loc_year_nestID) %>% 
  summarise(n_check = n(), max_int = max(int)) %>% 
  filter(n_check == 1)

obs_data <- obs_data %>% 
  filter(!loc_year_nestID %in% unlist(nest_one_check$loc_year_nestID))
years <- obs_data %>% 
  select(obsYear,year) %>% 
  distinct() %>% 
  arrange(year)

for(i in 1:nrow(obs_data)){
  y <- as.integer(obs_data[i,"obsYear"])
  int <- as.character(obs_data[i,"name"])
  if(int == "interval2"){
  sd <- as.integer(obs_data[i,"day_last_alive"]) # doy value
  ed <- sd+as.integer(obs_data[i,"interval"])
  }else{
    sd <- as.integer(obs_data[i,"day_found"]) # doy value
    ed <- sd+as.integer(obs_data[i,"interval"])
  }
  sn_dur <- snow_data %>% 
    filter(obsYear == y,
           doy <= ed & doy >= sd)
  if(nrow(sn_dur) == 0){
    obs_data[i,"mean_snow_cover"] <- 0
    
    obs_data[i,"p_days_no_snow_cover"] <- 1
    

    obs_data[i,"start_date_snow_cover"] <- 0
    print(paste("row",i,"has no snow data for",y,"and","days",sd,"-",ed))
  }else{
  obs_data[i,"mean_snow_cover"] <- mean(as.numeric(sn_dur$SnowCover_per))
  
  obs_data[i,"p_days_no_snow_cover"] <- length(which(sn_dur$SnowCover_per <= 0))/((ed-sd)+1)
  
  sn_dur <- snow_data %>% 
    filter(obsYear == y,
           doy == sd)
  
  obs_data[i,"start_date_snow_cover"] <- (as.numeric(sn_dur$SnowCover_per))
  }
}



obs_data$mean_snow_cover <- as.numeric(scale(obs_data$mean_snow_cover, center = FALSE))


obs_data <- obs_data %>% 
  filter(check == 1) %>% 
  mutate(nest = as.integer(factor(loc_year_nestID)))  #drops all dates for nest found (irrelevant)
#arrange(nest,int) 


nest_densities <- obs_data %>% 
  select(nest,n_shore_nests_near,n_goose_nests_near) %>% 
  distinct() %>% 
  arrange(nest)
# fit simple model --------------------------------------------------------


stan_data <- list(n_nests = max(obs_data$nest),
                  n_obs = nrow(obs_data),
                  y = obs_data$active,
                  nest = obs_data$nest,
                  interval = as.integer(obs_data$interval),
                  check = as.integer(obs_data$int-1),
                  n_neighbour = nest_densities$n_shore_nests_near,
                  snow_per = obs_data$mean_snow_cover,
                  use_likelihood = 1)


library(cmdstanr) # I prefer this interface to Stan - it's more up to date, and it gives nicer error messages

mod_base <- cmdstanr::cmdstan_model(stan_file = "base_model_irregular.stan")

fit<- mod_base$sample(data=stan_data,
                                             refresh = 500, 
                                             iter_sampling = 1000, 
                                             iter_warmup = 1000, 
                                             parallel_chains = 4)

summ_base <- fit$summary()

surv <- summ_base %>% filter(grepl("survival",variable))
snow <- summ_base %>% filter(grepl("snow",variable))
nest_density <- summ_base %>% filter(grepl("nest_density",variable))
sd_alpha <- summ_base %>% filter(grepl("sd_alpha",variable))
intercept <- summ_base %>% filter(grepl("intercept",variable))

ss <- summ_base %>% filter(grepl("s[",variable,fixed = TRUE))

pp <- summ_base %>% filter(grepl("p[",variable,fixed = TRUE))





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

#sum of effort hours per year
hours_sums <- rowSums(filtered_obs_hours[,-c(1:3)], na.rm = TRUE)
filtered_obs_hours$total_sum<-hours_sums

#now divide by the sum of foxes per row by the sum of obs hours per row
fox_indice <- scale(species_log_fox$total_sum/filtered_obs_hours$total_sum)
species_log_fox$fox_indice <- as.numeric(fox_indice)

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
lemming_sums <- (rowSums(filtered_species_log_lemming [,-c(1:4)], na.rm = TRUE))
filtered_species_log_lemming$total_sum<-lemming_sums


lemming_sum_aggregate<-aggregate(filtered_species_log_lemming$total_sum, list(filtered_species_log_lemming$obsYear), FUN = sum)
#This worked. 
#adding this beside the fox indice
lemming_indice <- scale(lemming_sum_aggregate$x/filtered_obs_hours$total_sum)
species_log_fox$lemming_indice <- as.numeric(lemming_indice)
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


fox_lemming_annual <- species_log_fox %>% 
  select(obsYear,fox_indice,lemming_indice) %>% 
  inner_join(years,by = "obsYear") %>% 
  arrange(year)
# compile into stan list structure ----------------------------------------

stan_data[["fox_a"]] <- as.numeric(fox_lemming_annual$fox_indice)
stan_data[["lemming_a"]] <- as.numeric(fox_lemming_annual$lemming_indice)
stan_data[["n_years"]] <- max(fox_lemming_annual$year)
stan_data[["year"]] <- obs_data$year
stan_data[["n_goose_neighbour"]] <- as.integer(nest_densities$n_goose_nests_near)


mod_full <- cmdstanr::cmdstan_model(stan_file = "full_model_irregular.stan")


fit_full<- mod_full$sample(data=stan_data,
                      refresh = 500, 
                      iter_sampling = 1000, 
                      iter_warmup = 1000, 
                      parallel_chains = 4)

summ_full <- fit_full$summary()

surv <- summ_full %>% filter(grepl("survival",variable))
snow <- summ_full %>% filter(grepl("snow",variable))
fox_lemming <- summ_full %>% filter(grepl("fox",variable))
lemming <- summ_full %>% filter(grepl("lemming",variable))
goose <- summ_full %>% filter(grepl("goose",variable))

nest_density <- summ_full %>% filter(grepl("nest_density",variable))
sd_alpha <- summ_full %>% filter(grepl("sd_alpha",variable))
intercept <- summ_full %>% filter(grepl("intercept",variable))

ss <- summ_model %>% filter(grepl("s[",variable,fixed = TRUE))

pp <- summ_model %>% filter(grepl("p[",variable,fixed = TRUE))


loo_full <- fit_full$loo()

y_rep <- fit_full$draws(variables = "y_rep",
                        format = "draws_matrix")

ppcheck <- bayesplot::ppc_bars(y = stan_data$y,
                               yrep = y_rep)

ppcheck

#now save this as an .rds
saveRDS(stan_data, "stan_data_list_all.rds")


