# set working directory
setwd("D:/Coaching/Business Intelligence/Data")


# read dataset regtest.csv
dat<-read.csv("regtest.csv")


# what is dat
class(dat)

# the sampling for learning and validation dataset will start now

# set seed for random number generation
myseed<-2


set.seed(myseed)


#check what seq_len() does
seq_len(nrow(dat))

# get the learning dataset with 70% records
dat1<-dat[sample(seq_len(nrow(dat)),0.7*nrow(dat),replace=F),]

# get the validation dataset with records of dat minus the ones in dat1
uniq<-setdiff(dat$pid,dat1$pid)
dat2<-dat[dat$pid %in% uniq,]

# write dat1 and dat2 as csv files back to working directory

write.csv(dat1,"regtest_s1.csv")



# but the above command gives also the row names in the new csv file, thus use the following
write.csv(dat1,"regtest_s1.csv",row.names=F)
write.csv(dat2,"regtest_s2.csv",row.names=F)

head(dat1,n=5)

# run linear regression to find out endurance
lm.r<-lm(endurance~age, data=dat1)
summary(lm.r)
anova(lm.r)

# plot the regression model
layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(lm.r)

# predict the data based on the model
lm.p<-predict(lm.r,newdata=dat2,level=0.95,interval="confidence")

# plot the endurance value along with the error in prediction
lm.diff<-lm.p[,1]-dat2$endurance
plot(lm.diff~dat2$endurance)

# draw a line at y=0, and also the difference vs actual values\
abline(h=0)
lines(smooth.spline(lm.diff~dat2$endurance),col="red")


# the score can be saved in a csv file
write.csv(lm.p,"regtest_pred.csv")

# however, to compare the result, one would need to compare it with the original data, and for this rownames will be required
# thus, save dat2 with rownames
write.csv(dat2,"regtest_s2_full.csv")

