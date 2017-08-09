#Crosstabs in R
ID <- seq(1:177)
Age <- sample(c("0-15", "16-29", "30-44", "45-64", "65+"), 177, replace = TRUE)
Sex <- sample(c("Male", "Female"), 177, replace = TRUE)
Country <- sample(c("England", "Wales", "Scotland", "N. Ireland"), 177, replace = TRUE)
Health <- sample(c("Poor", "Average", "Good"), 177, replace = TRUE)
Survey <- data.frame(Age, Sex, Country, Health)
head(Survey)

# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#The basic table types supported by crosstab() are:
  
#f requency - frequency count
#r ow.pct - proprotion within row
#c ol.pct - proportion within column
#j oint.pct - proportion within final 2 dimensions of table
#t otal.pct - proportion of entire table


# Frequency count
crosstab(Survey, row.vars = "Age", col.vars = "Sex", type = "f")

# Row percentages
crosstab(Survey, row.vars = "Age", col.vars = "Sex", type = "c")

# Joint percentages (sums to 100 within final two table dimensions)
crosstab(Survey, row.vars = c("Age", "Sex"), col.vars = "Health", type = "j")


library(ggplot2)

#------------------
# CREATE DATA FRAME
#------------------
df.team_data <- expand.grid(teams = c("Team A", "Team B", "Team C", "Team D")
                            ,metrics = c("Metric 1", "Metric 2", "Metric 3", "Metric 4", "Metric 5")
)

#df.team_data
# add variable: performance
set.seed(41)
df.team_data$performance <- rnorm(nrow(df.team_data))
summary(df.team_data)
head(df.team_data)
#inspect
nrow(df.team_data)



#---------------------------
# PLOT: heatmap
# - here, we use geom_tile()
#---------------------------

#ggplot() initializes a ggplot object. It can be used to declare the input data 
#frame for a graphic and to specify the set of plot aesthetics intended to be 
#common throughout all subsequent layers unless specifically overridden.
#ggplot() is used to construct the initial plot object, and is almost always followed by + to add component to the plot


ggplot(data = df.team_data, aes(x = metrics, y = teams)) +
  geom_tile(aes(fill = performance)) 

??"ggplot"


ds<-data("iris")
iris
reg<-lm(Sepal.Length~Petal.Length,data=iris)
coef(reg)
summary.lm(reg)
plot(iris$Sepal.Length,iris$Petal.Length)
abline(iris$Sepal.Length~iris$Petal.Length)
summary(reg)
summary(reg)
