#install.packages('lme4')
library(lme4)
model <- lmer(mpg ~ disp + (1 | cyl), mtcars)
#install.packages('influence.ME')
library(influence.ME)
infl <- influence(model, obs = TRUE)
cooks.distance(infl)
plot(infl, which = "cook")
cooksD_data<-as.data.frame(cooks.distance(infl)) 
cooksD_data_select<-cooksD_data[cooksD_data>0.1,drop=FALSE,]
cooksD_oultiers<-as.numeric(rownames(cooksD_data_select))
cooksD_oultiers
