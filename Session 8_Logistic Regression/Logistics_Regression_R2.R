#Logistic Regression
#Titanic Example
training.data.raw<-read.csv("train_titanic.csv",header=T,na.strings = c(""))
#each missing value would be coded as NA in above

#Check for the missing values
sapply(training.data.raw,function(x) sum(is.na(x)))

#variable cabin has lots of missing values

#check for unique values
sapply(training.data.raw, function(x) length(unique(x)))

#A Visual check on the Missing values
#install.packages("Amelia")
require('Amelia')
missmap(training.data.raw,main="Missing vs Observed")
#we see the cabin has lots of missing values
#Also, we are going to drop Ticket and PassengerId
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
head(data)

#Taking care of the missing values
#We will use the mean imputation approach
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm = T)

#As we know, while reading the CSV, R converts strings to categorical variables
#the constrasts function can be leveraged how R has dummified the categorical variables
contrasts(data$Sex)
sum(is.na(data$Embarked))
data<-data[!(is.na(data$Embarked)),]

nrow(data)

#Model fitting
train<-data[1:800,]
test<-data[801:889,]

model<-glm(Survived~.,family = binomial(link = 'logit'),data=train)
summary(model)

#interpretation of the model
#We can say that Sibsp, Parch, Fare, Embarked are not significant.
#As for teh statistical significance sex has lowest p-value suggesting a strong correlation of
#the passenger with the probability of being survived

anova(model,test = "Chisq")
#Code to analysis of the deviance. The difference between the null deviance and the residual deviance shows
#How our model performed as compared to the null model (model with only intercept).
#Wider the gap, better

#while there is no R2 equivalent in logistics regression, the Mcfadden R2 index can be used to access the fit of the model
#install.packages("pscl")
library(pscl)
pR2(model)
#The typical range for McFadden R2 is 0.2 to 0.4 but it depends on industry. For example in psychology the R2 value of 0.8 is considered to be very high while in Physics it is not.

##Accessing the predictability of the model
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results<-fitted.results[!is.na(fitted.results)]
test_Survived<-test$Survived[!is.na(test$Survived)]
misClasificError <- mean(fitted.results != test_Survived)
print(paste('Accuracy',1-misClasificError))

#Plotting the AUC and the TPR table
#install.packages('ROCR')
library(ROCR)
p<-predict(model,newdata = subset(test,select=c(2,3,4,5,6,7,8)),type="response")
p<-p[!is.na(p)]
pr<-prediction(p,test_Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#Creation of the confusion matrix
table(p>0.5,test_Survived)
library(caret)
#install.packages('caret')
