# set working directory
setwd("D:/Coaching/Business Intelligence/Data")


# read dataset regtest.csv
dat<-read.csv("regtest.csv")

head(dat)

plot(dat$age,dat$endurance)
lm.r<-lm(dat$age~dat$endurance)
summary(lm.r)
abline(lm.r)

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
lm.r<-lm(dat1$age~dat1$endurance)
lm.p<-predict(lm.r,newdata=dat1,level=0.95,interval="confidence")
lm.pr<-predict(lm.r,newdata=dat1,level=0.95,interval="predict")
summary(lm.p)
summary(lm.pr)
head(dat1)
print(lm.p)
