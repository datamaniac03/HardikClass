#Detecting Multicolinearity

body_fat<-read.csv("bodyfat-reduced.csv")
#Plot to get a look and feel of data
plot(body_fat)
#Calculation of the correlation matrix
mycorr<-cor(body_fat)
print(mycorr)
#Eigen SYstem Analysis
print(eigen(cor(body_fat))$values)
#if all the Eigen values are about the same value then we do not have much collinearity but if Eigen values are ordered
#from high to low with high difference then we have multicolinearity.

#Quantification of Eigen Values
max(eigen(cor(body_fat))$values)/min(eigen(cor(body_fat))$values)
#Rule of thumb - if we have this value of 100 or more than we have a significant multicolinearity
kappa(cor(body_fat),exact=T)

#The 3rd and the most effective way to detect the multicolinearity is the variance inflation method
model<-lm(body_fat~.,data=body_fat)
install.packages("car",dependencies=T)
library(car)
vif(model) #check here is to see if any of the VIF is > 5 or 10?
mean(vif(model)) #check is to see if the mean VIF is bigger than one

#Ridge Regression - Our Penecea to kill the multicollinearity
#Not a great tool that We have used

x1<-rnorm(20)
x2<-rnorm(20,mean=x1,sd=0.1)
cor(x1,x2)
y<-rnorm(20,mean=3+x1+x2,sd=0.2)
lm(y~x1+x2)$coef

#now let us try the ridge regression
library(MASS)
lm_seq<-seq(0,1,0.01)
fit<-lm.ridge(y~x1+x2,lambda=lm_seq)
select(fit) #Gives least GMV at 0.071
#Select looks for a least value of generalized coefficients. takes a few samples from the dataset fits the model and runs the test on the remaining dataset.
lm.ridge(y~x1+x2,lambda=0) #equivalent of Linear regression

lm.ridge(y~x1+x2,lambda=0.18)
#a lot closer than the OLS.
#useful only when we have a near perfect multicolinearity

#let us try this on a real dataset
bodyfat1<-read.csv("bodyfat-reduced.csv")
#need to standardize the data before you can use the ridge regression
bodyfat<-data.frame(scale(bodyfat1))
plot(bodyfat)
#calculate correlation
mycorr<-cor(bodyfat)
print(mycorr)
#Eigen SYstem Analysis
print(eigen(cor(bodyfat))$values)
#if all the Eigen values are about the same value then we do not have much collinearity but if Eigen values are ordered
#from high to low with high difference then we have multicolinearity.

#Quantification of Eigen Values
max(eigen(cor(bodyfat))$values)/min(eigen(cor(bodyfat))$values)
#Rule of thumb - if we have this value of 100 or more than we have a significant multicolinearity
kappa(cor(bodyfat),exact=T)

#Let us run the ridge regression on the data
lm_seq<-seq(0,1,0.01)
fit=lm.ridge(BodyFat~.,data=bodyfat,lambda = lm_seq)
#plot the fit to get a trace
plot(fit)
select(fit)
lm.ridge(BodyFat~.,data=bodyfat,lambda = 0.81)
plot(lm_seq,fit$GCV,main="GCV of the ridge Regression",type="l",xlab="Expression n(Lambda)",ylab="GCV")
lm.ridge(BodyFat~.,data=bodyfat,lambda = 0)
