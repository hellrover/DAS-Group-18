library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(MASS)
library(pscl)


data<-read.csv("dataset18.csv")
glimpse(data)
ggplot(data,aes(x=time_at_shelter))+
  geom_histogram()

###OLS model
model1<-lm(time_at_shelter~animal_type+intake_type+outcome_type+chip_status,data=data)
summary(model1)
points1<-get_regression_points(model1)
print(AIC(model1))

###Poisson regression
model2<-glm(time_at_shelter~animal_type+intake_type+outcome_type+chip_status, family = poisson(link = "log"), data=data)
summary(model2)

###Checking discrete parameter to see if poisson is suitable
alpha <- sum(residuals(model2, type = "pearson")^2) / model2$df.residual
print(paste("discrete parameter alpha:", round(alpha, 3)))

###Negative binominal regression
model3<-glm.nb(time_at_shelter~animal_type+intake_type+outcome_type+chip_status,data=data)
summary(model3)

###Residual plots
plot(x=points1$time_at_shelter_hat,y=points1$residual,
     xlab = "fitted value", ylab = "residual", main = "ols residual")
abline(h = 0, col = "red")

plot(model2$fitted.values, residuals(model2, type = "deviance"),
     xlab = "fitted value", ylab = "residual", main = "poisson residual")
abline(h = 0, col = "red")

plot(model3$fitted.values, residuals(model3, type = "deviance"),
     xlab = "fitted value", ylab = "residual", main = "nb residual")
abline(h = 0, col = "red")


model4<-glm.nb(time_at_shelter~intake_type+outcome_type+chip_status,data=data)
summary(model4)
plot(model4$fitted.values, residuals(model4, type = "deviance"),
     xlab = "fitted value", ylab = "residual", main = "nb residual")
abline(h = 0, col = "red")
