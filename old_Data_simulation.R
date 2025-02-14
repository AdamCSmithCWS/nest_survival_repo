## simulate data with known effects
library(tidyverse)
library(boot)


stan_data_real <- readRDS("stan_data_list_all.rds")

Nnests <- stan_data_real$Nnests
maxage <- stan_data_real$maxage

first_day <- stan_data_real$first_day_as_int_days


density_500m <- stan_data_real$density_500m
snow_per <- stan_data_real$snow_per

B_1 <- 1.5 # suggested intercept so that survival = 0.85
B_2_density <- 0.5
B_3_snow <- -0.5



s_sim <- vector("numeric",
            length = Nnests)

y_sim <- matrix(data = 0,
                nrow = Nnests,
                ncol = maxage)



for(i in 1:Nnests){
  
  ## simulated nest-level daily survival
  s_sim[i] <- inv.logit(B_1 + B_2_density*density_500m[i] + B_3_snow*snow_per[i])
  
  ## first day has to == 1
  y_sim[i,first_day[i]] <- 1
  # for following days nest is active, simulate daily status
  for(d in c((first_day[i]+1):min(maxage,(first_day[i]+30)))){ #setting the maximum last day as 30 days after the first day, because that is the maximum in teh real data
    if(y_sim[i,d-1] == 1){ # if the simulated nest is still active sample
      y_sim[i,d] <- rbinom(1,
                           size = 1,
                           prob = s_sim[i]) #binomial with size = 1 is the same as bernoulli (size = number of trials)
    }else{next}
  }
  
  
}



stan_data_sim <- stan_data_real
stan_data_sim[["y"]] <- y_sim



#  Then fit the model to the simulated data -------------------------------



stan_data_sim[["use_likelihood"]] <- as.integer(1) #need to set this to 1 if I am running the model. Set to 0 for prior predictive checks

library(cmdstanr) # I prefer this interface to Stan - it's more up to date, and it gives nicer error messages

mod_prep <- cmdstanr::cmdstan_model(stan_file = "make_stan_model_like_tobias_roth.stan")

sim_model_fit <- mod_prep$sample(data=stan_data_sim,
                                             refresh = 100, #default is 200, or 500
                                             iter_sampling = 1000, #just for initial attempts, but use the defaults when running for real - default =1000
                                             iter_warmup = 1000, #default = 1000
                                             parallel_chains = 4)#just for initial attempts, but use the defaults when running for real - default = 4


#check to see if the estimated b parameters match the simulated 
summ_model <- sim_model_fit$summary(variables = "b")


#testing to see if the simulated data looks like the real data:
#I ran the Data_prep_alr.R file to get the matrix y
row_sums_y<- rowSums(stan_data_real$y)
row_sums_y_sim<- rowSums(y_sim)
hist(row_sums_y)
hist(row_sums_y_sim)
#So these are the number of days each nest was active. 
#they look pretty similar in overall shape -- y falls off more quickly than y_sim

mean(row_sums_y)
mean((row_sums_y_sim))
#The means differ by about 2 days, which seems like a lot. 

#lets compare that to density

#scatter plot for simulated data
# Create a data frame with density and average survival
plot_data <- data.frame(density = density_500m, row_sums_y_sim= row_sums_y_sim)

# Use tapply to calculate mean survival for each density value
mean_survival_by_density <- tapply(plot_data$row_sums_y_sim, plot_data$density, mean)

plot_data <- plot_data %>% 
  group_by(factor(density)) %>% 
  mutate(mean_survival_by_density = mean(row_sums_y_sim))

# Convert the result to a data frame
plot_data_summary <- data.frame(density = as.numeric(names(mean_survival_by_density)),
                                mean_survival = as.vector(mean_survival_by_density))


# Scatter plot for real data
plot(plot_data_summary$density, plot_data_summary$mean_survival, 
     xlab = "Density", ylab = "Mean Average Survival",
     main = " y_sim: Mean Average Survival vs. Density", pch=21, bg="blue", cex=2)

# Create a data frame with density and average survival
plot_data_y <- data.frame(density = density_500m, row_sums_y = row_sums_y)

# Use tapply to calculate mean survival for each density value
mean_survival_by_density_y <- tapply(plot_data_y$row_sums_y, plot_data_y$density, mean)

# Convert the result to a data frame
plot_data_summary_y <- data.frame(density = as.numeric(names(mean_survival_by_density_y)),
                                mean_survival_y = as.vector(mean_survival_by_density_y))

# Scatter plot
plot(plot_data_summary_y$density, plot_data_summary_y$mean_survival_y, 
     xlab = "Density", ylab = "Mean Average Survival",
     main = "y: Mean Average Survival vs. Density", pch=19, bg="blue", cex=2)
#you can see that the mean survival days for density of 2 is 14 for y but for y_sim it is only 6.