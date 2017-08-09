#Missing Values in R Vectors
a<-c(1,2,3,4,5)
a[4]<-NA
is.na(a)
a[!is.na(a)]
na.omit(a)
################

#Removing missing values in R dataframes
#Step 1: Identify where NA are present in the dataframe
#Step 1.1: to Identify this, use functions like - head(), tail(), any(is.na()), colsums(is.na())

#Do some NA Imputation for the airquality data (copied into my.data)
my.data<-airquality
my.data[154,]<-c(NA)
my.data[,7]<-c(NA)
is.na(my.data)
##Identify if we have got NA
any(is.na(my.data))
#Get a look to the data to understand the number of observations and where NAs are located
str(my.data)
#once we do the above command we see the V7 is full of NAs and we might want to strip this before we move forward
#Stripping can be done as below
my.data<-my.data[1:6]
#we can alternatively use my.data[-7]

str(my.data)

#We still need to pop out the last row and for this we use:
my.data<-my.data[-154,]

#See if we have any NAs distributed across the data:
any(is.na(my.data))
sum(is.na(my.data))
colSums(is.na(my.data))

#First way to treat the missing values is to just eliminate it as shown below:
my.data.clean<-na.omit(my.data)
any(is.na(my.data.clean))
my.data.clean2<-my.data[complete.cases(my.data),]
any(is.na(my.data.clean2))

#2nd Way is to omit only in selected columns. Example:
my.data.clean3<-na.omit(my.data[-1])
nrow(my.data.clean3)


#If we have multiple columns instead of repeating the above steps for each we can use a more intelligent way as below:
my.data.clean4<-my.data[,colSums(is.na(my.data))<10]
nrow(my.data.clean4)
my.data.clean.final<-na.omit(my.data.clean4)
nrow(my.data.clean.final)


#Sometimes it might happen that the amount of data available for each of sample reprentation is so low that eliminating the record might make the entire sample bias. In that case it is always advisable to try and impute the missing values.
#Consider the data set for marks of 10 students as below:
math_m<-c(88,95,NA,76,68,99,98,70,NA,60)
mean(math_m[!is.na(math_m)])
median(math_m[!is.na(math_m)])
sd(math_m[!is.na(math_m)])
#We will start imputation but first we will create copy of original dataset
math_m1<-math_m
math_m2<-math_m


#Mean imputation
math_m1[is.na(math_m1)]=mean(math_m1[!is.na(math_m1)])
math_m1

#Median Imputation
math_m2[is.na(math_m2)]=median(math_m2[!is.na(math_m2)])
math_m2
math_m

#Regression Imputation
#Step 1: Creation of sample data
x<-seq(1:10)
y<-c(11,12,18,14,17,NA,NA,19,NA,27)
z<-c(19,11,2,14,20,4,9,10,18,1)
w<-c(1,4,7,10,3,5,7,6,6,9)
data<-data.frame(x,y,z,w)

#Step 2: Do the regression imputation
#Step 2.1: Look for the most correlated variable for Y (for which imputation needs to be done)
#Step 2.2: Create an indicator variable I such that, I=1 for complete cases and I=0 for cases which are incomplete
#Step 2.3: Fit a linear regression model Y on the most coorelated variable with Y
#Step 2.4: for each i=0 Impute or predict for the missing value using the above regression


#Step 2.1: Look for the most correlated variable for Y 
cor(data,use = "complete.obs") #complete.obs ensures NA are ignored for finding correlation
symnum(cor(data,use="complete.obs"))

#Step 2.2: Create an indicator variable I such that, I=1 for complete cases and I=0 for cases which are incomplete
Ind<-function(t)
{
  x<-dim(length(t))
  x[which(is.na(t))]<-0
  x[which(!is.na(t))]<-1
  return(x)
}
data$I<-Ind(data$y)

#Step 2.3: Fit a linear regression model Y on the most coorelated variable with Y
lm(y~x,data=data)
summary(lm(y~x,data=data))

#y=9.7432+1.509x
for(i in 1:nrow(data))
{
  if(is.na(data$y[i]))
    {
    data$y[i]=9.7432+1.509*data$x[i]
  
  }
}

