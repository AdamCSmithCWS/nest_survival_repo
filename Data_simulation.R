## simulate data with known effects
library(tidyverse)
library(boot)


stan_data_real <- readRDS("stan_data_list_all.rds")

Nnests <- stan_data_real$Nnests
maxage <- stan_data_real$maxage

first_day <- stan_data_real$first_day_as_int_days


density_50m <- stan_data_real$density_50m
snow_per <- stan_data_real$snow_per

B_1 <- 1.7 # suggested intercept so that survival = 0.85
B_2_density <- 0.5
B_3_snow <- -0.5



s_sim <- vector("numeric",
            length = Nnests)

y_sim <- matrix(data = 0,
                nrow = Nnests,
                ncol = maxage)



for(i in 1:Nnests){
  
  ## simulated nest-level daily survival
  s_sim[i] <- inv.logit(B_1 + B_2_density*density_50m[i] + B_3_snow*snow_per[i])
  
  ## first day has to == 1
  y_sim[i,first_day[i]] <- 1
  # for following days nest is active, simulate daily status
  for(d in c((first_day[i]+1):(first_day[i]+30))){ #setting the maximum last day as 30 days after the first day, because that is the maximum in teh real data
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
                                             iter_sampling = 100, #just for initial attempts, but use the defaults when running for real - default =1000
                                             iter_warmup = 500, #default = 1000
                                             parallel_chains = 4)#just for initial attempts, but use the defaults when running for real - default = 4


#check to see if the estimated b parameters match the simulated 
summ_model <- sim_model_fit$summary(variables = "b")



