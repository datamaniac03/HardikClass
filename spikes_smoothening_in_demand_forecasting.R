#Problem on specific spike smoothening: Machine Learning Exercise.
Link to problem statement:

https://docs.google.com/document/d/19KWkkJ7ZddLbnz52uKpMNh1q-QcVsDn3_yxdI16Wu8s/edit

#Solution Code:-

#downloaded the data file for case1 as data2.xlsx

require(oce)
dats<-read.xlsx("data2.xlsx",1)
colnames(dats)<-c("week","demand")
demands<-na.omit(dats$demand)
weeks<-na.omit(dats$week)
#de_dem<-despike(demands,reference=c("median","smooth","trim"),n=4,k=7,1.020,9.661,replace=c("reference","NA"))
plot(weeks,demands,type='l')
lines(weeks, despike(demands), col='red')
lines(weeks, despike(demands, reference="smooth"), col='darkgreen')
lines(x, despike(y, reference="trim", min=1.02, max=9.661), col='blue')
legend("topright", lwd=1, col=c("black", "red", "darkgreen", "blue"),
       legend=c("raw", "median", "smooth", "trim"))