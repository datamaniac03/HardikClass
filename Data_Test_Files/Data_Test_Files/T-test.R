setwd("D:/Coaching/Business Intelligence/Data")
dat<-read.csv("Cereals.csv")
#The T-test can be done only between one predictor and outcome variable

head(dat)

# independent 2-group t-test
#t.test(y~x) # where y is numeric and x is a binary factor
#Example:
t.test(dat$calories~dat$type)
#By default this calls for Welch's T-test where the null hypothesis is that the mean of the two population are equal

# independent 2-group t-test
#t.test(y1,y2) # where y1 and y2 are numeric
str(dat)
t.test(dat$calories,dat$fat)

#Paired t-test
#The purpose of the test is to determine whether there is statistical evidence 
#that the mean difference between paired observations on a particular outcome 
#is significantly different from zero. The Paired Samples t Test is a parametric 
#test. This test is also known as: Dependent t Test.

#Example: Suppose you are interested in evaluating the effectiveness of a company 
#training program. One approach you might consider would be to measure the 
#performance of a sample of employees before and after completing the program, 
#and analyze the differences using a paired sample t-test.

t.test(dat$vitamins,dat$protein,paired=TRUE) # where y1 & y2 are numeric

#One sample t-test
t.test(dat$vitamins,mu=28.25)

#The F-test can be done between all the predictor and outcome variable
#Assumptions of F-Test -
#Linearity Assumption: Considers the linear relationship between the target and set of predictable variables
#Null Hypothesis - No Linear relationship exists - all regression coefficients are zero
#Alternate Hypothesis - atleast one regression coefficients is non-zero
#A small value would confirm the null hypothesis

#While the T-test covers comparison of the means, the f-test covers the comparison of variances
y<-rnorm(100, mean=1, sd=2)
x<-rnorm(100, mean=1, sd=5)
var.test(lm(x~1),lm(y~1))
