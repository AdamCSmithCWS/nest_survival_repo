#run stan model like tobias roth
setwd("/Users/isaacfinkelstein/Documents/Carleton/courses/bayesian/research project/nest_survival_repo")

library(rstan)

run_stan_model_like_tobias<- stan(file = "stan_model_like_tobias_roth.stan", data=clean_nest_fate_data,
                                  chains=5, iter=2500, control=list(adapt_delta=0.9), verbose = FALSE)
