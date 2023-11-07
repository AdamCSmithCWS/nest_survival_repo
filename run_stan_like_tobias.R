#run stan model like tobias roth
setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo")

# library(rstan)
#
#
# ### the function `stan()` accepts a named list as data input
#
#
# run_stan_model_like_tobias<- stan(file = "stan_model_like_tobias_roth.stan",
#                                   data=stan_data,
#                                   # chains=5, trust the defaults
#                                    iter=600, #just for testing, use the defaults afterwards
#                                   warmup = 500,
#                                   verbose = TRUE)
#

stan_data <- readRDS("stan_data_list_50m.rds")

## try cmdstanr: https://mc-stan.org/cmdstanr/articles/cmdstanr.html
library(cmdstanr) # I prefer this interface to Stan - it's more up to date, and it gives nicer error messages

mod_prep <- cmdstanr::cmdstan_model(stan_file = "make_stan_model_like_tobias_roth.stan")

run_stan_model_like_tobias<- mod_prep$sample(data=stan_data,
                                             refresh = 100, #default is 200, or 500
                                             iter_sampling = 100, #just for initial attempts, but use the defaults when running for real - default =1000
                                             iter_warmup = 500, #default = 1000
                                             parallel_chains = 4)#just for initial attempts, but use the defaults when running for real - default = 4

summ_model <- run_stan_model_like_tobias$summary()
View(summ_model)


library(boot)
inv.logit(0.009489755)#for density (b2) #this may change as my model changes

