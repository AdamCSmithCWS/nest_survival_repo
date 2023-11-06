data {
  int<lower=0> Nnests; //number of nests
  array[Nnests]int<lower=1> first_day_as_int_days; //for each of nNests, day (integer) it was first observed
  array[Nnests]int<lower=0> last_day_as_int_days; //for each of Nnests, the day it was last observed
  int<lower=0> maxage; //maximum of last
  array[Nnests, maxage]int<lower=0, upper=1> y; //indicator of alive nests
  array[Nnests]int<lower=0> density_50m; //a covariate of the nests
  array[Nnests] int<lower=0> snow_per;
}

parameters {
  vector[3] b; // I only need two priors right? because I have two predictors
}

model {
  array[Nnests, maxage-1]real S; //survival probability

  for(i in 1:Nnests){
    for(t in first_day_as_int_days[i]: (last_day_as_int_days[i]-1)){
      S[i,t] = inv_logit(b[1] + b[2]*density_50m[i] +b[3]*snow_per[i]); // I haven't done b[3]*age[t] here!
    }
  }

  //priors

  //likelihood
  for(i in 1:Nnests) {

    for (t in (first_day_as_int_days [i] +1): last_day_as_int_days [i]){
      y[i,t] ~bernoulli(y[i,t-1]*S[i,t-1]);
    }
  }
}

