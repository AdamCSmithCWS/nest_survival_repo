data {
  int<lower=0> Nnests; //number of nests
  array[Nnests]int<lower=1> first_day_as_int_days; //for each of nNests, day (integer) it was first observed
  array[Nnests]int<lower=0> last_day_as_int_days; //for each of Nnests, the day it was last observed
  int<lower=0> maxage; //maximum of last
  array[Nnests, maxage]int<lower=0, upper=1> y; //indicator of alive nests
  array[Nnests]int<lower=0> density_50m; //a covariate of the nests
  array[Nnests]int<lower=0> snow_per;
  int<lower=0,upper=1> use_likelihood;
}

parameters {
  vector[3] b; // I only need two priors right? because I have two predictors
}
transformed parameters {
  //array[Nnests, maxage-1]real<lower=0,upper=1> S; //survival probability
  array[Nnests]real<lower=0,upper=1> S; //survival probability

  for(i in 1:Nnests){
    //for(t in first_day_as_int_days[i]: (last_day_as_int_days[i]-1)){
      S[i] = inv_logit(b[1] + b[2]*density_50m[i] + b[3]*snow_per[i]);
    //}
  }
}


model {
  
  //priors
  b[1]~ normal(1.5,1); //the mean is on the inverse logit scale - so 1.5 is a daily survival of 0.82. 
    b[2]~ normal(0,0.1);
      b[3]~ normal(0,0.1);
  
  //likelihood
  if(use_likelihood){
  for(i in 1:Nnests) {
  
    for (t in (first_day_as_int_days[i] +1): last_day_as_int_days[i]){
      y[i,t] ~ bernoulli(y[i,t-1]*S[i]);
    }
  }
  }
}

generated quantities {
  
  // vector[Nnests] surv_pred;
  vector[3] survival_overall_pred;
  real diff_survival;
  array[Nnests, maxage]int<lower=0,upper=2> y_rep;
  // for(i in 1:Nnests){
  //  surv_pred[i] = inv_logit(b[1] + b[2]*density_50m[i] +b[3]*snow_per[i]);
  // }
  // 
  survival_overall_pred[1] = inv_logit(b[1] + b[2]*0 +b[3]*0);
  survival_overall_pred[2] = inv_logit(b[1] + b[2]*1 +b[3]*0);
  survival_overall_pred[3] = inv_logit(b[1] + b[2]*2 +b[3]*0);
  
  diff_survival = survival_overall_pred[3] - survival_overall_pred[1];
  
  for(i in 1:Nnests) {
    for (t in 1:first_day_as_int_days [i]){
      y_rep[i,t] = 2;
    }
  y_rep[i,first_day_as_int_days[i]] = 1;
    for (t in (first_day_as_int_days [i] +1): last_day_as_int_days [i]){
      y_rep[i,t] = bernoulli_rng(y_rep[i,t-1]*S[i]);
    }
    
    for (t in (last_day_as_int_days [i]+1):maxage){
      y_rep[i,t] = 0;
    }
    
  }
  
}

