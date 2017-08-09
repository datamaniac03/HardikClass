#1. Load the data and run numerical and graphical summaries
#2. Split the data into training and testing data
#3. Fit a logistic regression model when training data set
#4. Use the fitted model to do the predictions for the test data
#5. Create the confusion matrix and compute the misclassification rate

#Get the dataset Smarket (S&P 500 stock index, 1250 observations, 9 variables)
install.packages('ISLR')
require('ISLR')
df<-Smarket
names(df)

#Get the summary of the data
summary(Smarket)
?(Smarket)


#get the corrrelation
cor(df[,-9])
#only significant correlation is between the year and volume
plot(df$Year~df$Volume)
#We will fit a logistic regression model to predict the direction using Lag 1 through 5 and volume.
#glm function fits generalized linear models, a class that includes logistic regression which has syntax similar to lm except for argument
#family=binomial to tell R that we are required to run a logistic regression rather than some other type of generalized regression model

glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=df,family = "binomial")

summary(glm.fit)

