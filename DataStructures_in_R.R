#Vectors and Factors
gpa<-c(3.45,3.97,2.98,4)
gpa[1]
gpa[-2]

first_nam<-c("Alice","bob","Maria","john")
pass<-c(TRUE,FALSE,TRUE,FALSE)
str(first_nam)
str(pass)
gender<-c("F","M","F","M")
str(gender)
gender<-factor(gender,levels=c("M","F"))
str(gender)
fcolor<-c("black","blue","red","red")
fcolor<-factor(fcolor)
str(fcolor)

#Sample Function
print(sample(1:100))
print(sample(1:3, size=3, replace=TRUE))  # same as previous line
print(sample(c(2,5,3), size=4, replace=TRUE))
print(sample(1:5, size=10, prob=c(0.3,0.2,0.1,0.1,0.2), replace=TRUE))

#Faithful
#In the data set faithful, develop a 95% prediction interval of the eruption duration for the waiting time of 80 minutes.

attach(faithful)
head(faithful)
eruption.lm<-lm(eruptions~waiting)
newdata<-data.frame(waiting=80)
predict(eruption.lm,newdata,interval="confidence")
#The 95% prediction interval of the eruption duration for the waiting time of 80 minutes is between 3.1961 and 5.1564 minutes.


#rm(list=ls())

ls()
df1<-data.frame(first_nam,gender,gpa,pass,fcolor)
nrow(df1)
for(obj in 1:nrow(df1))
{
  df1$pass[obj]=TRUE
if(df1$gpa[obj]==2.98)
  {
  df1$pass[obj]=FALSE
  }
}
df1
df1[2,c(1,3)]
df1[1,1]
df1[,4]
df1[c(2,3),]
str(df1)
#If you note the above carefully, you will see the first_nam by default has been converted to a factor
#we will see how to bypass that
df2<-data.frame(first_nam,gender,gpa,pass,fcolor, stringsAsFactors = FALSE)

##Import data into R
#R needs to know where the data is. Good practice is to create a folder in the working directory called data and put it there
#how to identify the working directory
getwd()
#how to set the working directory
#Two ways
#1. from Menu --> Session --> Set Working Directory
#2. using the setwd as below
setwd("D:/Coaching/Business Intelligence/Course/Data/")
df3<-read.csv("usedcars.csv", stringsAsFactors = FALSE)
df3$transmission<-factor(df3$transmission)
print(df3)
print(str(df3))
head(df3)
ycars<-subset(df3,color %in% "Yellow")
ycars<-subset(df3,color=="Yellow")
subset(df3,mileage>100000)
subset(df3,transmission="Auto")  
??subset
table(df3$transmission)
round(table(df3$transmission)/nrow(df3),2)
pie(table(df3$transmission))
hist(df3$mileage)
boxplot(df3$mileage,horizontal = TRUE)
plot(df3$mileage~df3$transmission)
str(tapply(df3$mileage,df3$model,mean))
n <- 17
fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
str(df3)

##Multivariate variables
plot(df3$mileage~df3$transmission)
boxplot(df3$mileage)
tapply(df3$mileage,df3$transmission,mean)
tapply(df3$price,df3$transmission,mean)
plot(df3$price~df3$mileage)
str(df3)
pairs(df3[,-c(2,5)])
cor(df3[,-c(2,5,6)])
tapply(df3$price,df3$color,mean)
aggregate(price~transmission,data=df3,FUN=mean)
aggregate(price~transmission+color,data=df3,FUN=length)
round(table(df3$transmission,df3$color)/length(df3$transmission),2)

