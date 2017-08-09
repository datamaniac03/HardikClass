hardik.data <- floor(runif(10,10,30))
print(hardik.data)
ajinkya.data<- findInterval(hardik.data,c(5,15,25,30)) 
print(ajinkya.data)
cut(hardik.data, breaks=c(5, 15, 25, 30), labels = c("Low","Mid", "HIGH"))

mydata<-read.csv("D:/Coaching/Business Intelligence/Course/Session 4/hsb2.csv")
print(mydata)
mydata$racel<-cut(mydata$race,breaks=c(0,1,2,3,4), labels=c("hispanic","asian","african-american","white"))
print(mydata$racel)



#Linear Regression in R:
#Assumptions in Linear Regression
#There are four principal assumptions which justify the use of linear regression models for purposes of inference or prediction:
 # (i) linearity and additivity of the relationship between dependent and independent variables:
  #(a) The expected value of dependent variable is a straight-line function of each independent variable, holding the others fixed.
  #(b) The slope of that line does not depend on the values of the other variables.
  #(c) The effects of different independent variables on the expected value of the dependent variable are additive.
# (ii) Statistical independence of the errors (in particular, no correlation between consecutive errors in the case of time series data)
# (iii) Homoscedasticity (constant variance) of the errors
# (a) versus time (in the case of time series data)
# (b) versus the predictions
# (c) versus any independent variable
#(iv) Normality of the error distribution.  

#Why Linearity is important
Y<-c(1,4,9,16,25,36,49,64,81,100,121,144,169,196)
X<-seq(1:14)
print(Y)
print(X)
lm(Y[1:4]~X[1:4])
summary(lm(Y[1:4]~X[1:4]))

df<-data.frame(Y,X)
df$predY<-(5*df$X-5)
print(df)
df$predE<-((df$Y-df$predY)/df$Y)*100
print(df$predE)


#Why should we assume that the effects of different independent variables on the expected value of the dependent variable are additive?  
#Again, this is a strong assumption. It implies that the marginal effect of one 
#independent variable (i.e., its slope coefficient) does not depend on the 
#current values of other independent variables. Why? Because it's conceivable 
#that one independent variable could amplify the effect of another, or that its 
#effect might vary systematically over time.

#Normal distribution of error: why errors of a linear model are independently and identically are assumed to be normally distributed?
#This assumption is often justified by appeal to the Central Limit Theorem of 
#statistics, which states that the sum or average of a sufficiently large number 
#of independent random variables--whatever their individual distributions--
#approaches a normal distribution. Much data in business and economics and 
#engineering and the natural sciences is obtained by adding or averaging 
#numerical measurements performed on many different persons or products or 
#locations or time intervals. Insofar as the activities that generate the 
#measurements may occur somewhat randomly and somewhat independently, 
#we might expect the variations in the totals or averages to be somewhat 
#normally distributed. 

#Violation of variable interdependence- Durbin-Watson Test
Y<-c(1,4,9,16,25,36,49,64,81,100)
X<-seq(1:10)

#To do this test you need lmtest package
#install.packages('lmtest',dependencies = TRUE)
require(lmtest)
dat<-data.frame(X,Y)
dwtest(lm(dat$Y~dat$X),iterations = 15,exact = TRUE)


#Detection for Heteroscedasticity
#Violations of Homoscedasticity: The assumption of homoscedasticity is central to most of the linear models. Homoscedasticity describes a situation in which the error term (that is, the "noise" or random disturbance in the relationship between the independent variables and the dependent variable) is the same across all values of the independent variables
#The problem that heteroscedasticity presents for regression models is simple. 
#A simple linear regression model tries to minimize residuals and in turn 
#produce the smallest possible standard errors.  By definition OLS regression 
#gives equal weight to all observations, but when heteroscedasticity is present 
#the cases with larger disturbances have more "pull" than other observations.  
#The coefficients from OLS regression where heteroscedasticity is present are 
#therefore inefficient but remain unbiased.  In this case, weighted least 
#squares regression would be more appropriate, as it down weights those 
#observations with larger disturbances.

#Diagnoses: To detect heteroscedasticity look at a plot of residual verses predicted values or in case of time series data, a plot of residual vs. time. In our example the residual verses predicted values plot is as below for linear as well as non-linear relationship data:
#How to fix it: Try to apply transformation on dependent as well as independent variables. Example if in the above non-linear data we apply logarithmic transformation then the graph comes out to be as follows post regression. The graph below clearly shows a linear relationship between dependent variable (Y) and independent variable (X) post applying log transformation.


#Violation of Normality: Weak Assumption if you only want to minimize the mean square error
#Test for Normality - Anderson Darling Test
#One thing to note in AD test is that the null hypothesis stats that the distribution is normal
#Package - https://cran.r-project.org/web/packages/nortest/nortest.pdf

dat<-rnorm(100)
print(dat)
ad.test(dat)
#install.packages('nortest')
require(nortest)
dat2<-rexp(100)
ad.test(dat2)

#Other Example:
#https://rexplorations.wordpress.com/2015/08/11/normality-tests-in-r/

#Generating 10k points of data and arranging them into 100 columns
x<-rnorm(10000,10,1)
dim(x)<-c(100,100)

#Generating a simple normal quantile-quantile plot for the first column
#Generating a line for the qqplot
qqnorm(x[,1])
qqline (x[,1], col=2)

abline(col=3)
