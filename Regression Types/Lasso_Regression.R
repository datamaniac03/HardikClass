#install.packages("glmnet")
library(glmnet)
set.seed(1)
x1 <- rnorm(30)
x2 <- rnorm(30)
x3 <- rnorm(30)
X <- matrix( c(x1, x2, x3), byrow = F, ncol = 3)
y <- 3 + 4*x1 + 3*x2 + 5*x3 + rnorm(30)
fit <-glmnet(x = X, y = y, alpha = 1) 
# different values of alpha return different estimators, alpha = 1 is the lasso.
plot(fit, xvar = "lambda")
#so ?? is the penalty or the Lagrange multiplier if you prefer and is always positive. 
#Setting ??=0
#yields the familiar minimization of squared residuals while for greater values, 
#some of the coefficients will be set to zero. As ????????????????, all the coefficents will be set 
#to zero.
#This is exactly what this plot shows then, the coefficient path for different values of 
#lambda. For reasons beyond me the creators of this package have opted to present lambda 
#on the log scale, thus values between zero and one are now negative.
#Pick the value of lambda you like and you can extract the coefficients with the command
coef(fit, s = 0.3) # s is the value of lambda
#locate a point on the plot
log(0.3)
#Cross validation and selection of minimum prediction error
crossval <-  cv.glmnet(x = X, y = y)
plot(crossval)
penalty <- crossval$lambda.min #optimal lambda
penalty #minimal shrinkage
#fit1<-lm(y~X)
fit1 <-glmnet(x = X, y = y, alpha = 1, lambda = penalty ) #estimate the model with that
coef(fit1)
