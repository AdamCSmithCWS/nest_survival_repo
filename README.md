# nest_survival_repo

Project to estimate the effects of numerous factors on the nest survival of arctic nesting shorebirds.

The model is a known-fate model that uses a logistic regression structure to estimate the effect of the number of neighbouring shorebird nests, fox density, lemming density, and other factors on the surival of shorebird nests at the East Bay study site. 

The model is fit in Stan.

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

Additional: 	
- We will want to get an idea of the distributions of the different species. Do any species aggregate more than others? Are they all the same?
 	
-	Write manuscript





