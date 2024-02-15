# nest_survival_repo
Directed studies course: bayesian stats: nest survival project


Code files:
Data_prep_alt.R – this is a cleaner (and better) version of the data preparation
Data_simulation – seems to work
-	We have simulated the model and it converges, with no effect for both density and snow (this is okay because we simulated the data to have no effect, right?). 
make_stan_model_like_tobias_roth.stan: - stan model for regression without fox variable
make_stan_model_like_tobias_roth_fox_interaction.stan:
-	Includes the fox interaction – (interaction between foxes and the effect of density).



Models:
Model 1:
Predictor: shorebird nest density
Response: nest survival
Controls: density of geese, Sabine’s gull proximity, snow cover

Model 2:
Predictor: shorebird nest density*fox abundance
Response: nest survival
Controls: density of geese, Sabine’s gull proximity, snow cover

Model 3:
Predictor: shorebird nest density*fox abundance
Response: nest survival
Controls: lemmings, density of geese, Sabine’s gull proximity, snow cover
(the fox interaction may only matter when lemmings are scarce)



Things still to do:
-	Get final dataset (year 2022). 
-	Add fox variable to stan_data
-	Run the models
  Set density to 500m
  Could also run the model at different density calculations (e.g. 200m) – maybe include that in the supplementary files. 
-	Model inference:
  Posterior distributions of parameters
  Derived parameters
  Graphical summaries
-	Write manuscript




