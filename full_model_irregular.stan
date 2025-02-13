data {
  int<lower=1> n_nests; //number of nests
  int<lower=1> n_obs; // number of observations (nest checks)
  int<lower=1> n_years; // number of years 
  array[n_obs]int<lower=0,upper=1> y; // status of a nest at each nest check
  array[n_obs]int<lower=1,upper=n_nests> nest; // nest indicator
  array[n_obs]int<lower=0> interval; // days since last check for this nest, will = 0 if check == 0
  array[n_obs]int<lower=1,upper=n_years> year; // status of a nest at each nest check
  array[n_obs]int<lower=0,upper=1> check; // indicator to track subsequent nest checks
  // if 0 for first check of a nest (first check following previous nest discovery)
  // if 1 for second check if next still active at first check
  
  vector[n_years] fox_a; // annual fox count
  vector[n_years] lemming_a; // annual lemming counts
  
  vector[n_nests] n_neighbour; // number of shorebird nests within buffer distance
  vector[n_nests] n_goose_neighbour; // number of goose nests within buffer distance
  vector[n_obs] snow_per; // mean snow cover over the interval
  int<lower=0,upper=1> use_likelihood; // if 0, then samples from prior
}

parameters {
  real intercept; // also the mean of the nest specific intercepts
  real nest_density; // effect of number of neighbouring shorebird nests
  real snow; // effect of scaled (not centered) snow cover
  real goose_density; // effect of number of goos nest neighbourds
  real lemming; // annual lemming count effect
  real fox; // annual fox count effect
  real fox_nest; // interaction of fox and lemming counts
  vector[n_nests] alpha_raw;// nest intercepts uncentered random effect (will be centered on intercept)
  real<lower=0> sd_alpha; //sd of nest random effect
}

transformed parameters {
  array[n_nests]real<lower=0,upper=1> s; //constant daily survival probability for each nest
  array[n_obs]real<lower=0,upper=1> p; //interval*nest survival probability
  vector[n_nests] alpha;// centered nest intercepts
  
  alpha = sd_alpha*alpha_raw;
  
    for(i in 1:n_obs){
      s[nest[i]] = inv_logit(intercept + 
      nest_density*n_neighbour[nest[i]] + 
      snow*snow_per[i]+
      goose_density*n_goose_neighbour[nest[i]] + 
      fox*fox_a[year[i]] +
      lemming*lemming_a[year[i]] +
      fox_nest*fox_a[year[i]]*n_neighbour[nest[i]]  +
      alpha[nest[i]]);
    p[i] = s[nest[i]]^interval[i]; //adjusts from daily survival to interval survival
  }
  

}


model {
  
  //priors
  intercept ~ normal(1.5,2); //the mean is on the inverse logit scale - so 1.5 is a daily survival of 0.82. 
  nest_density ~ normal(0,1);
  snow ~ normal(0,1);
  goose_density ~ normal(0,1);
  fox ~ normal(0,1);
  lemming ~ normal(0,1);
  fox_nest ~ normal(0,1);
  alpha_raw ~ normal(0,1);
  sum(alpha_raw) ~ normal(0,0.001*n_nests); // sum to zero constraint
  sd_alpha ~ student_t(3,0,1);




  //likelihood
  if(use_likelihood){
  for(i in 1:n_obs) {
      y[i] ~ bernoulli(p[i]); // can ignor previous status because only included if previous status was active
  }
  }
}

generated quantities {
  

  vector[3] survival_overall_pred;
  real diff_survival;
  array[n_obs]int<lower=0,upper=1> y_rep;
  vector[n_obs] log_lik; 

  survival_overall_pred[1] = inv_logit(intercept + nest_density*0 +snow*0);
  survival_overall_pred[2] = inv_logit(intercept + nest_density*1 +snow*0);
  survival_overall_pred[3] = inv_logit(intercept + nest_density*2 +snow*0);
  
  diff_survival = survival_overall_pred[3] - survival_overall_pred[1];
  
  for(i in 1:n_obs){
   if(check[i]){
      y_rep[i] = bernoulli_rng(y_rep[i-1]*p[i]);
      log_lik[i] = bernoulli_lpmf(y[i] | p[i]*y[i-1]);
     }else{
      y_rep[i] = bernoulli_rng(p[i]);
      log_lik[i] = bernoulli_lpmf(y[i] | p[i]);
     
     }
  }

}

