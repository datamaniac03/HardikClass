rm(list=ls())

iris_org<-iris
head(iris_org)

set.seed(500)
gp<-runif(nrow(iris_org))
iris_org<-iris_org[order(gp),]
print(iris_org)

#Creation of datasets
train_ds<-iris_org[1:130,]
train_ds<-train_ds[-5]
test_ds<-iris_org[131:150,]
test_ds<-test_ds[-5]
print(train_ds)
print(test_ds)


#creation of CLs
train_o<-iris_org[1:130,5]
test_o<-iris_org[131:150,5]
print(train_o)


#Running the KNN
m1<-knn(train = train_ds,test = test_ds,cl = train_o,k = sqrt(nrow(iris_org)))

#Correctness
table(test_o,m1)
