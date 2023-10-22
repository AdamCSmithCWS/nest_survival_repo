data {
  int<lower=0> Nnests; //number of nests
  // Stan will not accept "." within a variable name, use underscores
  // it's good practice to use snake case or camel case variables names everywhere
  // R allows "." but most other languages do not. read up on programming variable names
  // e.g., https://www.freecodecamp.org/news/snake-case-vs-camel-case-vs-pascal-case-vs-kebab-case-whats-the-difference
  // real Mayfield_days_formula[Nnests]; //how many days the nest was active
  // the model requires input data that for each nest reflects the
  // first day and the last day each nest was active
  // can you make your data fit this format?
  int<lower=0> first[Nnests]; // for each of nNests, day (integer) it was first observed
  int<lower=0> last[Nnests]; // for each of nNests, day it was last observed
  int<lower=0> maxage; //maximum of last?
  int<lower=0> y[Nnests, maxage]; //indicator of alive nests
  int<lower=0> X_of_eggs[Nnests]; //a covariate of the nests
}

parameters {
  vector[2] b; // I only need two priors right? because I have two predictors
}

model {
  // real S[Nnests, Mayfield_days_formula, maxage-1]; //survival probability
  // there are too too many dimensions in the line defining S
  // inside the square brackets, you've got three values separated by commas
  // that defins a 3-dimensional array, not the 2-dimensional array that you
  // call in the line below ("S[i,t] = inv_logit...").
  real S[Nnests, maxage-1]; //survival probability

  for(i in 1:Nnests){
    for(t in first[i]: (last[i]-1)){
      S[i,t] = inv_logit(b[1] + b[2]*X_of_eggs);
    }
  }

  //priors

  //likelihood
  for(i in 1:Nnests) {
    for (t in (first[i] + 1): last[i]){
      y[i,t] ~bernoulli(y[i,t-1]*S[i,t-1]);
    }
  }
}

