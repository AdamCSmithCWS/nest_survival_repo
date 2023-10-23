data {
  int<lower=0> Nnests; //number of nests
  int<lower=0> first_day_as_int_days[Nnests]; //for each of nNests, day (integer) it was first observed = days since 1/1/1070
  int<lower=0> last_day_as_int_days[Nnests]; //for each of Nnests, the day it was last observed = days since 1/1/1970
  int<lower=0> maxage; //maximum of last (is 17740 because it's days since 1/1/1970)
  int<lower=0> y[Nnests, maxage]; //indicator of alive nests
  int<lower=0> x_of_eggs[Nnests]; //a covariate of the nests
}

parameters {
  vector[2] b; // I only need two priors right? because I have two predictors
}

model {
  real S[Nnests, maxage-1]; //survival probability

  for(i in 1:Nnests){
    for(t in first_day_as_int_days[i]: (last_day_as_int_days[i]-1)){
      S[i,t] = inv_logit(b[1] + b[2]*x_of_eggs[i]); // I haven't done b[3]*age[t] here!
    }
  }
  
  //priors
  b[1]~normal(0,1.5);
  b[2]~normal(0,1);
  
  //likelihood
  for(i in 1:Nnests) {
    for (t in (first_day_as_int_days [i] +1): last_day_as_int_days [i]){
      y[i,t] ~bernoulli(y[i,t-1]*S[i,t-1]);
    }
  }
}

