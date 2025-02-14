# nest_survival_repo

Project to estimate the effects of numerous factors on the nest survival of arctic nesting shorebirds.

The model is a known-fate model that uses a logistic regression structure to estimate the effect of the number of neighbouring shorebird nests, fox density, lemming density, and other factors on the surival of shorebird nests at the East Bay study site. 

The model is fit in Stan.



Code files:
Data_prep_and_and_model_fit.R – updated script to prepare data and fit a full model 



Models:
full model:
Predictor: shorebird nest density*fox abundance
Response: nest survival
Controls: lemmings, density of geese,  snow cover
Excluded: Sabine’s gull proximity,



Things still to do:

  Could also run the model at different density calculations (e.g. 200m) – maybe include that in the supplementary files.
 	
-	Model inference:
  Posterior distributions of parameters
  Derived parameters
  Graphical summaries

Additional: 	
- We will want to get an idea of the distributions of the different species. Do any species aggregate more than others? Are they all the same?
 	
-	Write manuscript





