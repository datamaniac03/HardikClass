rent<-read.csv("Rent.csv")
str(rent)
head(rent)
plot(rent$Rent~rent$Income)
#plot(rent$Income~rent$Rent)
cor(rent$Income,rent$Rent)
attach(rent)
lm.r<-lm(Income~Rent)
summary(lm.r)
abline(lm.r,col="Red",lty=3, lwd=2)
lm.r
#Income=16716.752+2.45*Rent

predict(lm.r,data.frame("Rent"=2000))

predict(lm.r,data.frame("Rent"=2000),interval="confidence")

#Regression Coefficient - R square
#Coefficient of determination or coefficient of multiple determination
#Refer the R_Square.xlsx from here


#Multiple Linear Regression
#To access the dataset, first install the alr3 package in R's Package Installer
#... and then refresh the dataset list within Data Manager. 
#Yo0u should then see fuel2001 as an available dataset that you can load. 
#The variable names are slightly different to the video but you will be able to figure that out! Hope that helps ...???

#Types of Regression:
#Linear Regression - Linear Regression establishes a relationship between dependent variable (Y) and one or more independent variables (X) using a best fit straight line (also known as regression line).
#It is represented by an equation Y=a+b*X + e, where a is intercept, b is slope of the line and e is error term. This equation can be used to predict the value of target variable based on given predictor variable(s).

#Logistic Regression - Logistic regression is used to find the probability of event=Success and event=Failure. We should use logistic regression when the dependent variable is binary (0/ 1, True/ False, Yes/ No) in nature. Here the value of Y ranges from 0 to 1 and it can represented by following equation.
#odds= p/ (1-p) = probability of event occurrence / probability of not event occurrence
#ln(odds) = ln(p/(1-p))
#logit(p) = ln(p/(1-p)) = b0+b1X1+b2X2+b3X3....+bkXk
# "why have we used log in the equation?".
#Since we are working here with a binomial distribution (dependent variable), we need to choose a link function which is best suited for this distribution. And, it is logit function. In the equation above, the parameters are chosen to maximize the likelihood of observing the sample values rather than minimizing the sum of squared errors (like in ordinary regression).


#Polynomial regression - Power of one of the independent variables is more than 1

#Stepwise regression - used to automatically include/eliminate the features and thereby having optimal model with least number of variable

#Ridge Regression - Used when data suffers from Multicolinearity (high correlation between the independent variables) - effort is to reduce the error due to bias
