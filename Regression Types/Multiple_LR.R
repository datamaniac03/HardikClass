#Multiple Linear Regression
# Multiple Linear Regression Example 

#Create data
x1 <- rnorm(30)
x2 <- rnorm(30)
x3 <- rnorm(30)
mydata <- data.frame(x1, x2, x3)
y <- 3 + 4*x1 + 3*x2 + 5*x3

#Choose the Linear Regression Model
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
fit1 <- lm(y ~ x1 + x2 + x3, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)
library(DAAG)
cv.lm(data=mydata,fit,m=3)

#Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results
