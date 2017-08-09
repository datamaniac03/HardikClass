#Two key objetives of regression modelling
#Estimate the effect of one or more covariates while adjusting the possible confounding effects
#Prediction of the outcome for the next set of similar objects
#Here model selection can be useful but best predictions are usually obtained by averaging over a number
#of top models
#Bayesian Model Averaging does a good job at this and allows to do the following
#See what covariates are in the best models, with a model list provided that is ordered from best to worst.Line up the coefficient estimates from the different models so that one can
#easily see what estimates change as variables enter end exit the models,good for investigating confounding.
#Automatically perform model averaging.

#Difference between the AIC and BIC criterion
#AIC criterion - calculated as AIC = n ln(SSE)−n ln(n) + 2p. Recall that it tends to be 
#good for complex models, less good in finding simple models.Tends to overfit.
#In statistics, the Bayesian information criterion (BIC) or Schwarz criterion (also SBC, SBIC)
#is a criterion for model selection among a finite set of models; the model with the lowest 
#BIC is preferred. It is based, in part, on the likelihood function and it is closely 
#related to the Akaike information criterion (AIC).
#When fitting models, it is possible to increase the likelihood by adding parameters, 
#but doing so may result in overfitting. Both BIC and AIC attempt to resolve this problem 
#by introducing a penalty term for the number of parameters in the model; the penalty term 
#is larger in BIC than in AIC.

#We will now look at a series of examples where we will compare the two main techniques, the AIC and BIC, through several examples
#Example 1: Large number of covariates, null model is true
#we will create a large data set with 1000 cases and 30 independent variables, but where no variable in fact is related to the outcome
rates <- round(seq(.1, .9, length.out=15), 2)
x1 <- rbinom(1000, 1, rates[1])
x2 <- rbinom(1000, 1, rates[2])
x3 <- rbinom(1000, 1, rates[3])
x4 <- rbinom(1000, 1, rates[4])
x5 <- rbinom(1000, 1, rates[5])
x6 <- rbinom(1000, 1, rates[6])
x7 <- rbinom(1000, 1, rates[7])
x8 <- rbinom(1000, 1, rates[8])
x9 <- rbinom(1000, 1, rates[9])
x10 <- rbinom(1000, 1, rates[10])
x11 <- rbinom(1000, 1, rates[11])
x12 <- rbinom(1000, 1, rates[12])
x13 <- rbinom(1000, 1, rates[13])
x14 <- rbinom(1000, 1, rates[14])
x15 <- rbinom(1000, 1, rates[15])

#similarly create 15 normally distributed variables without loss of generality, mean=0 and sd=1 throughout
x16 <- rnorm(1000)
x17 <- rnorm(1000)
x18 <- rnorm(1000)
x19 <- rnorm(1000)
x20 <- rnorm(1000)
x21 <- rnorm(1000)
x22 <- rnorm(1000)
x23 <- rnorm(1000)
x24 <- rnorm(1000)
x25 <- rnorm(1000)
x26 <- rnorm(1000)
x27 <- rnorm(1000)
x28 <- rnorm(1000)
x29 <- rnorm(1000)
x30 <- rnorm(1000)
y <- rbinom(1000, 1, 0.5)
example1.dat <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                           x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,
                           x21, x22, x23, x24, x25, x26, x27, x28, x29, x30)
head(example1.dat)
#install.packages('BMA')
library('BMA')
output <- bic.glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 +
                    x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 +
                    x15 + x16 + x17+ x18 + x19 + x20 + x21 +
                    x22 + x23 + x24 + x25 + x26 +
                    x27 + x28 + x29 + x30, glm.family="binomial",
                  data=example1.dat, maxCol = 31)
summary(output)
#Posterior probability of each of 10 best models (rest very small by
# comparison, so are omitted, change value of OR to see them)
output$postprob
output$label
# For each of 30 variables, probability they should be in the model
# Note that largest is 70.6%, quite small.
# Note straightforward interpretation compared to p-values
output$probne0

# Bayesian model averaged means for each variable. All very near 0 except intercept.
output$postmean

# Bayesian model averaged SDs for each variable.
output$postsd
# For each of top 10 models (in this case), model by model estimates
# This is where you can check for confounding, very conveniently.
output$mle
#Overall, we can see that the BIC does very well, picking out the correct model
# with high probability.

# Let’s see how AIC compares here.
output.aic <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 +
                      + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 +
                      + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 +
                      + x29 + x30, data = example1.dat, family = "binomial")
summary(output.aic)

step.aic <- step(output.aic)
# So eight variables make the final model
# according to the AIC
# As expected, the model is too large.
# Can also just ask for a summary of the AIC
# output

summary(step.aic)
#Overall conclusion: As expected, when small models are “correct”, the BIC does
#better compared to the AIC, the latter tending to provide too many variables in the
#“best” model

#Example 2: Large number of co-variates but some are important
#dataset with 500 cases and 20 variables
#10 related to outcome and 10 are not
# Create 10 dichotomous random independent variables, each with a
# different rate of ‘‘positive’’ outcomes (i.e., 1’s).
rates <- round(seq(.1, .9, length.out=10), 2)
rates
x1 <- rbinom(500, 1, rates[1])
x2 <- rbinom(500, 1, rates[2])
x3 <- rbinom(500, 1, rates[3])
x4 <- rbinom(500, 1, rates[4])
x5 <- rbinom(500, 1, rates[5])
x6 <- rbinom(500, 1, rates[6])
x7 <- rbinom(500, 1, rates[7])
x8 <- rbinom(500, 1, rates[8])
x9 <- rbinom(500, 1, rates[9])
x10 <- rbinom(500, 1, rates[10])
x11 <- rnorm(500)
x12 <- rnorm(500)
x13 <- rnorm(500)
x14 <- rnorm(500)
x15 <- rnorm(500)
x16 <- rnorm(500)
x17 <- rnorm(500)
x18 <- rnorm(500)
x19 <- rnorm(500)
x20 <- rnorm(500)
inv.logit.rate <- exp(x1 + x2 + x3 + x4 + x5 + x11 + x12 + x13 + x14 +x15)/
  (1+ exp(x1 + x2 + x3 + x4 + x5 + x11 + x12 + x13 + x14 +x15))
y <- rbinom(500, 1, inv.logit.rate)
example2.dat <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                           x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)

output <- bic.glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 +
                  x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 +
                    x15 + x16 + x17+ x18 + x19 + x20, glm.family="binomial",
                  data=example2.dat)
summary(output)
# Posterior probability of each of 14 best models (rest very small by
# comparison, so are omitted, change value of OR to see them)
output$postprob
# What variables were in each of above 41 models
output$label
output$probne0
output$mle
output$se
# Note that neither mle estimates nor their SDs change much
# from model to model, so no evidence of confounding. This
# is as expected, as all variables independent here,
# so in fact there was no confounding.
# Overall, we can see that the BIC does reasonably well, but misses the
# true correct model. So, evidence that BIC models sometimes too small.

# Let’s see if AIC does better here:
output.aic <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 +
                    x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 +
                    x19 + x20, data = example2.dat, family = "binomial")
summary(output.aic)
step.aic <- step(output.aic)
# So 11 variables make the final model
# according to the AIC, including 9
# that should be there, but also 2 that should not be there.
summary(step.aic)
# So BIC best model a bit too small, AIC much too large, and still
# also missed x1.
# Overall, not clear cut which was better here, but BIC
# better for predictions, and allows easy checking of confounding.

