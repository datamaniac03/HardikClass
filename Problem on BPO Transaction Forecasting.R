Problem on BPO Transaction Forecasting:

Link to Problem statement:
https://docs.google.com/document/d/1eqlNcE1bkWZVe2cIo0KmWVeBSADW0DPwilSlNzh4i-Y/edit


Solution Code:
###############################
#Reading data from the excel file or CSV downloaded
require(xlsx)
#dat<-read.csv("case7.csv")
dat<-read.xlsx("Case Study 7 - Data.xlsx",sheetIndex = 1)
colnames(dat)<-c("time","transactions")
dat1<-na.omit(dat) #removing data with na values

#to identify if there exists a relation (linear) between the % of time spent and the total no. of tickets resolved
gr<-glm(dat1$transactions~dat1$time)
print(summary(gr))
dat1$div<-((dat1$transactions-1224)/dat1$time) #1224 is the coefficient of intercept
#plot(dat1$transactions~dat1$time)
print(plot(dat1$time,dat1$div)) #after removing the intercept data we are left with only those transactions which are affected by time and hence we are ploting those against the time




print(plot(dat1$time,dat1$transactions,xlab="time",ylab="transactions"))

print(head(dat1))
d <- dist(dat1$transactions, method = "euclidean")
fit <- hclust(d, method="ward")
plot(fit)
groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border="red")



#install.packages("mclust")
library(mclust)
fit <- Mclust(dat1$transactions)
plot(fit)
summary(fit)
#############################
Answers to Questions:

How would you quantify the relationship between the time spent on the software and the number of transactions closed?

As can be seen from the glm model summary, there exist a linear relation between the no. of transactions closed vs. the time spend on the software.

What would be the range of improvement possible by increasing the time spent on the software? What should be the time spent on the software to maximise productivity?

As seen from the graph, the increase in productivity happens till 86% of time spent on the software. the range of improvement is from 1229 to 4243 transactions.

Do you see different clusters of employees based on this data (the ones who are more efficient than others)?

This is answered with the help of cluster analysis, we do see few clusters of employees based on the time they spent on the no. of tickets they solve vs. the time they spent on the software. the same is achieved through cluster analysis.