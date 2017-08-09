#First Graph - Histogram
#What is a Histogram?
#A histogram is an accurate graphical representation of the distribution of 
#numerical data. It is an estimate of the probability distribution of a 
#continuous variable (quantitative variable).
#It is a kind of bar graph. To construct a histogram, the first step is to "bin"
#the range of values-that is, divide the entire range of values into a series of
#intervals-and then count how many values fall into each interval. 
#The bins are usually specified as consecutive, non-overlapping intervals of a 
#variable. The bins (intervals) must be adjacent, and are often 
#(but are not required to be) of equal size

#install.packages("histogram")
library(histogram)
data("iris")
summary(iris)
head(iris)
sl<-iris$Sepal.Length
hist(sl)

#A Density Plot visualises the distribution of data over a continuous interval 
#or time period. This chart is a variation of a Histogram that uses kernel 
#smoothing to plot values, allowing for smoother distributions by smoothing out 
#the noise. The peaks of a Density Plot help display where values are concentrated
#over the interval
sl.d<-density(sl)
plot(sl.d)
lines(sl.d)

#Plotting density graph over histogram. Set the frequency to false as otherwise 
#it might push the density graph below considering former is based on frequency 
#while later on probability
hist(sl,freq = FALSE)
lines(sl.d)

#An advantage Density Plots have over Histograms is that they're better at 
#determining the distribution shape because they're not affected by the number 
#of bins used (each bar used in a typical histogram). 
#A Histogram comprising of only 4 bins wouldn't produce a distinguishable 
#enough shape of distribution as a 20-bin Histogram would. 
#However, with Density plots this isn't an issue.
#An advantage Density Plots have over Histograms is that 
#they're better at 
#determining the distribution shape because they're not affected by the number of bins used (each bar used in a typical histogram). A Histogram comprising of only 4 bins wouldn't produce a distinguishable enough shape of distribution as a 20-bin Histogram would. 
#However, with Density plots this isn't an issue.



#2nd Graph - Boxplot
boxplot(sl)
#a simple way of representing statistical data on a plot in which a rectangle 
#is drawn to represent the second and third quartiles, usually with a vertical 
#line inside to indicate the median value. 
#The lower and upper quartiles are shown as horizontal lines either side of the 
#rectangle.

summary(sl)

sl.b<-boxplot(sl)
summary(sl.b)
names(sl.b)
?boxplot
sl.b$stats[3]

#3rd Graph - Barplot
barplot(sl)
?barplot
#just takes the data as it is and plots it unlike smoothening by Histogram

#4th Graph - ScatterPlot
#a graph in which the values of two variables are plotted along two axes, 
#the pattern of the resulting points revealing any correlation present

pairs(iris[1:5])
pairs(iris[1:2])

qqnorm(sl)
qqline(sl)

#More than one graph in 1
par(mfrow=c(2,2))
boxplot(sl)
qqnorm(sl)
hist(sl,freq = FALSE)
lines(sl.d)
barplot(sl)
#Reset Par
par(mfrow=c(1,1))

library(lattice)
head(iris)
bwplot(Sepal.Length~Sepal.Width,data=iris)
dotplot(Sepal.Length~Sepal.Width,data=iris)
xyplot(Sepal.Length~Sepal.Width|Species,data=iris)


library(psych)

#install.packages("psych")
pairs.panels(iris)

ds<-rnorm(100)