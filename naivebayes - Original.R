# Using Naive Bayes in the Iris dataset to classify in terms of Species
# Load dataset & library
#it is always useful to have sorted data in Naive Bayes because each alteration between the independent variable will cause one error
#For example, if we start with sertosa as first observation, it will predict all the next as Sertosa till the time it encounters a virginica, post this it will predict all upcoming as virginica.

library(klaR)

di<-read.table("Iris.csv",header=T,sep=",")

# Training and Validation Partitions
set.seed(1020)
part<-sample(1:nrow(di),ceiling(2/3*nrow(di)),replace=F)
trn<-di[part,]
val<-di[-part,]

# Develop model over training set
m.tr<-NaiveBayes(Species~.,dat=trn)

# Plot the model
plot(m.tr)

# Predict the performance of the model using validation set
m.vl<-predict(m.tr,dat=trn)


# Print the confusion matrix for training set
print(table(trn$Species,m.vl$class))

# Print the confusion matrix for validation set
m.vl<-predict(m.tr,val)
print(table(val$Species,m.vl$class))


# Load a new dataset which does not have the classification
pd<-read.table("irispred.csv",header=T,sep=",")


# use the previous model to predict the class

print(predict(m.tr,pd))
predict(m.tr,pd)


head(pd)
