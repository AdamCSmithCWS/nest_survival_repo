data {
  int<lower=1> n_nests; //number of nests
  int<lower=1> n_obs; // number of observations (nest checks)
  array[n_obs]int<lower=0,upper=1> y; // status of a nest at each nest check
  array[n_obs]int<lower=1,upper=n_nests> nest; // nest indicator
  array[n_obs]int<lower=0> interval; // days since last check for this nest, will = 0 if check == 0
  array[n_obs]int<lower=0,upper=1> check; // indicator to track first observations and subsequent checks
  // above is 0 for first observations of a nest (day of nest discovery)
  // and 1 for all other observations for a given nest
  

  vector[n_nests] n_neighbour; // number of shorebird nests within buffer distance
  vector[n_obs] snow_per; // mean snow cover over the interval
  int<lower=0,upper=1> use_likelihood;
}

parameters {
  real intercept; // also the mean of the nest specific intercepts
  real nest_density; // 
  real snow; // 
  vector[n_nests] alpha_raw;// nest intercepts uncentered random effect (will be centered on intercept)
  real<lower=0> sd_alpha; //sd of nest random effect
}

transformed parameters {
  array[n_obs]real<lower=0,upper=1> s; //daily survival probability
  array[n_obs]real<lower=0,upper=1> p; //interval specific survival probability
  vector[n_nests] alpha;// centered nest intercepts
  
  alpha = sd_alpha*alpha_raw;
  
    for(i in 1:n_obs){
      s[i] = inv_logit(intercept + nest_density*n_neighbour[nest[i]] + alpha[nest[i]] + snow*snow_per[i]);
    p[i] = s[i]^interval[i]; //adjusts from daily survival to survival over the number of days in interval
  }
  
  // for(i in 1:n_obs){
  //   p[i] = s[nest[i]]^interval[i]; //adjusts from daily survival to interval survival
  // }
}


model {
  
  //priors
  intercept ~ normal(1.5,2); //the mean is on the inverse logit scale - so 1.5 is a daily survival of 0.82. 
  nest_density ~ normal(0,1);
  snow ~ normal(0,1);
  alpha_raw ~ normal(0,1);
  sum(alpha_raw) ~ normal(0,0.001*n_nests); // sum to zero constraint
  sd_alpha ~ student_t(3,0,1);




  //likelihood
  if(use_likelihood){
  for(i in 1:n_obs) {
  if(check[i]){
      y[i] ~ bernoulli(p[i]);
    }
  }
  }
}

generated quantities {
  
  // vector[n_nests] surv_pred;
  vector[3] survival_overall_pred;
  real diff_survival;
  array[n_obs]int<lower=0,upper=1> y_rep;
  //vector[n_obs] log_lik; 
  // for(i in 1:n_nests){
  //  surv_pred[i] = inv_logit(intercept + nest_density*n_neighbour[i] +snow*snow_per[i]);
  // }
  // 
  survival_overall_pred[1] = inv_logit(intercept + nest_density*0 +snow*0);
  survival_overall_pred[2] = inv_logit(intercept + nest_density*1 +snow*0);
  survival_overall_pred[3] = inv_logit(intercept + nest_density*2 +snow*0);
  
  diff_survival = survival_overall_pred[3] - survival_overall_pred[1];
  
  for(i in 1:n_obs){
    if(check[i]){
      y_rep[i] = bernoulli_rng(y_rep[i-1]*p[i]);
    }else{
    y_rep[i] = 1;
    }
  }
  // 
  // for(i in 1:n_nests) {
  //   for (t in 1:first_day_as_int_days [i]){
  //     y_rep[i,t] = 2;
  //   }
  // y_rep[i,first_day_as_int_days[i]] = 1;
  //   for (t in (first_day_as_int_days [i] +1): last_day_as_int_days [i]){
  //     y_rep[i,t] = bernoulli_rng(y_rep[i,t-1]*s[i]);
  //   }
  //   
  //   for (t in (last_day_as_int_days [i]+1):maxage){
  //     y_rep[i,t] = 0;
  //   }
  //   
  // }
  // 
}

