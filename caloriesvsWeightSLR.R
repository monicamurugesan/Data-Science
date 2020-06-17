#Load calories datasets.

calories_consumed <-read.csv(file.choose())
calories_consumed
# Exploratory data analysis
summary(calories_consumed)
plot(calories_consumed)
attach(calories_consumed)
#Scatter plot
plot(Weight.gained..grams.,Calories.Consumed)
#Correlation Coefficient (r)
cor(Weight.gained..grams.,Calories.Consumed)
# Simple Linear Regression model
reg <-lm(Weight.gained..grams.~Calories.Consumed)
summary(reg)
pred <-predict(reg)
plot(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
var(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(calories_consumed))
sqrt(mean(sum(reg$residuals^2))) #RMSE
confint(reg,level=0.95)
predict(reg,interval = "predict")
# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data=calories_consumed,aes(x=Weight.gained..grams.,y=Calories.Consumed))+geom_point(color='red')+geom_line(color='blue',data=calories_consumed,aes(x=Weight.gained..grams.,y=Calories.Consumed))



#Log
plot(log(Weight.gained..grams.),Calories.Consumed)
cor(log(Weight.gained..grams.),Calories.Consumed)
str(calories_consumed)
reg_l <-lm(Weight.gained..grams.~log(Calories.Consumed))
summary(reg_l)
predict(reg_l)
reg_l$residuals
sqrt(sum(reg_l$residuals^2)/sum(calories_consumed))
confint(reg_l,interval=0.95)
predict(reg_l,interval="predict")





####expotential#####
plot(log(Calories.Consumed),Weight.gained..grams.)
cor(log(Calories.Consumed),Weight.gained..grams.)
reg_exp <-lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(reg_exp)
reg_exp$residuals
logat <-predict(reg_exp)
new <-exp(logat)
error=calories_consumed$Weight.gained..grams.-new
sqrt(sum(reg_exp$residualsr^2)/nrow(calories_consumed))
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")


####Square####
plot(Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed,log(Weight.gained..grams.))
reg_sqrt <-lm(Weight.gained..grams.~sqrt(Calories.Consumed))
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="confidence")

plot(reg_sqrt)


#Quad
# plot(Weight.gained..grams.,log(Calories.Consumed))
# plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
# plot(Weight.gained..grams.*Weight.gained..grams.,log(Calories.Consumed))
# cor(Weight.gained..grams.,Calories.Consumed)
# cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
# cor(Weight.gained..grams.*Weight.gained..grams.,log(Calories.Consumed))
# regquad <-lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed*Calories.Consumed))
# summary(regquad)
# sum(sqrt(regquad$residuals^2)/nrow(calories_consumed))
# log<-predict(regquad)
# exp<-exp(log)
# error <-calories_consumed$Calories.Consumed-exp
# sqrt(sum(error^2)/nrow(calories_consumed))
# confint(regquad,level=0.95)
# predict(regquad,interval="confidence")
# 
# ggplot(data = calories_consumed, aes(x =Calories.Consumed + I(Calories.Consumed^2), y = log(Weight.gained..grams.))) + 
#   geom_point(color='blue') +
#   geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=log))
# 
# 
# reg3 <-lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed*Calories.Consumed)+I(Calories.Consumed*Calories.Consumed*Calories.Consumed))
# summary(reg3)
# log1 <-predict(reg3)
# exp3 <-exp(log1)
# confint(reg3,level=0.95)
# predict(reg3,interval="predict")
# 
# 
# ggplot(data = calories_consumed, aes(x = Calories.Consumed+ I(Calories.Consumed^2) + I(Calories.Consumed^3), y = Weight.gained..grams.)) + 
#   geom_point(color='blue') +
#   geom_line(color='red',data =calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y=exp3))