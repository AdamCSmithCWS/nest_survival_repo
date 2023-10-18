#load data
load("nest_surv_data.rda")
str(datax)

datax$y[is.na(datax$y)] <- 0

#compile stan model
library(rstan)
fit_stan_model <- stan(file= "practice_Stan_model.stan", data=datax, chains = 5,
                   iter=2500, control=list(adapt_delta=0.9), verbose = FALSE)

#check convergence and explore performance of the Markov chains
library(shinystan)
launch_shinystan(fit_stan_model)

print(fit_stan_model)

#effect plot
bsim <- as.data.frame(stan_model)
nsim<- nrow(bsim)

newdat <- data.frame(age=seq(1, datax$maxage, length=100))
newdat$age.z <- (newdat$age-mean(1:datax$maxage))/sd((1:datax$maxage))
Xmat <- model.matrix(~age.z, data=newdat)
fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] <- plogis(Xmat%*%as.numeric(bsim[i,c(1,3)]))
newdat$fit <- apply(fitmat, 1, median)
newdat$lwr <- apply(fitmat, 1, quantile, prob=0.025)
newdat$upr <- apply(fitmat, 1, quantile, prob=0.975)

plot(newdat$age, newdat$fit, ylim=c(0.8,1), type="l",
     las=1, ylab="Daily nest survival", xlab="Age [d]")
lines(newdat$age, newdat$lwr, lty=3)
lines(newdat$age, newdat$upr, lty=3)
#produces the estimated daily nest survival probability in relation to nest age
#dotted lines are 95% uncertainty interevals of the regression line


#next a model for irregular nest controls (when the nest could have died anytime between now and last check)
#create a variable when the nest was last seen alive (lastlive)

#time when nest was last observed alive
lastlive <-apply(datax$y, 1, function(x) max(c(1:length(x)) [x==1]))
#time when nest was last checked (alive or dead)
lastcheck <- lastlive+1

#when nest was observed alive at the last check, then lastcheck equals
lastcheck[lastlive==datax$last] <- datax$last[lastlive==datax$last]
datax1 <- list(Nnests=datax$Nnests,
               lastlive=lastlive,
               lastcheck= lastcheck,
               first=datax$first,
               cover=datax$cover,
               age=datax$age,
               maxage=datax$maxage)
#time between last alive and fist seen dead (=lastcheck)
datax1$gap<-datax1$lastcheck-datax1$lastlive

#make the stan model

#run stan
fit_irr_stan_model <-stan(file="prac_irr_nest_survival.stan", data = datax1,
                          chains=5, iter=2500, control=list(adapt_delta=0.9),
                          verbose=FALSE)
