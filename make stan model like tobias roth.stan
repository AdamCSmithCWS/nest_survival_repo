data {
  int<lower=0> Nnests; //number of nests
  real Mayfield_days..formula.[Nnests]; //how many days the nest was active
  int<lower=0> maxage; //maximum of mayfield days formula
  int<lower=0> y[Nnests, maxage]; //indicator of alive nests
  int<lower=0> X_of_eggs[Nnests]; //a covariate of the nests
}

parameters {
  vector[2] b; // I only need two priors right? because I have two predictors
}

model {
  real S[Nnests, Mayfield_days..formula., maxage-1]; //survival probability

  for(i in 1:Nnests){
    for(t in first [i]: (last[i]-1)){
      S[i,t] = inv_logit(b[1] + b[2]*X_of_eggs);
    }
  }
  
  //priors
  
  //likelihood
  for(i in 1:Nnests) {
    for (t in (first [i] +1): last [i]){
      y[i,t] ~bernoulli(y[i,t-1]*S[i,t-1]);
    }
  }
}

