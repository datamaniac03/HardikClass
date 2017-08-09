#How to Normalize the data in R?
#The KNN algorithm works on the following principle:
#1.Calculates the distance between the features of unidentified class object from the members of the identified group
#2. Finds the top Kth which are at the least distance from the observations
#3. Does a voting to select the class of unidentified class object
#The caution is to rescale the features with big values as they can affect the distance calculation in R
#The way to do this is to Normalize the data in R

require(xlsx)
setwd("D:/Coaching/Business Intelligence/Course/Session 6_Linear Regression/KNN")
df<-read.xlsx("KNN_Excel.xlsx",sheetName = "Sheet3")
head(df)
#Let us assume the data across features was very varied - done as below for manipulation
df$Sepal.Length<-df$Sepal.Length*10

#Its a good practice to normalize the data as below
df$Sepal.Length<-with(df, (Sepal.Length-min(Sepal.Length))/(max(Sepal.Length)-min(Sepal.Length)))
df$Sepal.Width<-with(df, (Sepal.Width-min(Sepal.Width))/(max(Sepal.Width)-min(Sepal.Width)))
#or
df$srp<-(df$Sepal.Width-min(df$Sepal.Width))/(max(df$Sepal.Width)-min(df$Sepal.Width))

df<-df[-4]

#Let us consider the actual dataset
df<-iris
head(df)
table(iris$Species)
#data in above table is not random its organized by species

#need to randomize the data
#Set seed will use the 9850 to create random numbers. 
set.seed(9850)
gp<-runif(nrow(df))
df2<-df[order(gp),]

#need to rescale the data now as the next step
summary(df2)

#use the normalize function
normalize<-function(x)
{
    return ((x-min(x))/(max(x)-min(x)))
}
#Apply the normalize function to multiple columns
df3<-as.data.frame(lapply(df2[,c(1,2,3,4)],normalize))
#df3$Species<-df2$Species
head(df3)
#df3<-df3[-4]

#Create the training and test data frame
head(df3)
nrow(df3)
#training dataset
iris_train<-df3[1:130,] 
nrow(iris_train)
#test dataset
iris_test<-df3[131:150,]
nrow(iris_test)
#train target
iris_train_target<-df2[1:130,5]
nrow(iris_train_target)
#test target
iris_test_target<-df2[131:150,5]

#iris_train<-iris_train[-4]
#iris_test<-iris_test[-4]

summary(iris_train)
summary(iris_test)
summary(iris_train_target)
summary(iris_test_target)

#Run the KNN algorithm
require(class)
sqrt(nrow(df3))
m1<-knn(train=iris_train,test = iris_test,cl = iris_train_target,sqrt(nrow(df3)))
head(m1)
table(iris_test_target,m1)
