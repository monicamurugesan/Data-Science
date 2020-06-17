#Load dataset

delivery_time <-read.csv(file.choose())
View(delivery_time)
delivery_time
#exploratory data analysis
summary(delivery_time)
attach(delivery_time)
cor(Delivery.Time,Sorting.Time)
sd(Delivery.Time)
sd(Sorting.Time)
var(Sorting.Time)
mean(Delivery.Time)
mean(Sorting.Time)
regd <-lm(Delivery.Time ~ Sorting.Time)
plot(Sorting.Time,Delivery.Time)
plot(regd)
summary(regd)
sum(regd$residuals)
mean(regd$residuals)
sqrt(sum(regd$residuals^2)/nrow(delivery_time))
confint(regd,level=0.95)
predict(regd,interval="predict")
library(ggplot2)
plot(delivery_time)
ggplot(data=delivery_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='red')+geom_line(color='blue',data=delivery_time,aes(x=Sorting.Time,y=Delivery.Time))

plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
str(delivery_time)
reg_l <-lm(Delivery.Time~log(Sorting.Time))
summary(reg_l)
plot(reg_l)
predict(reg_l)
reg_l$residuals
sqrt(sum(reg_l$residuals^2)/sum(calories_consumed))
confint(reg_l,interval=0.95)
predict(reg_l,interval="predict")

cor(log(Sorting.Time),Delivery.Time)
reg_exp <-lm(log(Delivery.Time)~Sorting.Time)
plot(reg_exp)
summary(reg_exp)
reg_exp$residuals
logat <-predict(reg_exp)
new <-exp(logat)
error=delivery_time$Sorting.Time-new
sqrt(sum(reg_exp$residualsr^2)/nrow(calories_consumed))
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

#Quad


plot(Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, Sorting.Time)

cor(Delivery.Time*Delivery.Time,Sorting.Time)

plot(Delivery.Time*Delivery.Time, log(Sorting.Time))

cor(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time*Delivery.Time, log(Sorting.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(Delivery.Time ~ Sorting.Time+ I(Sorting.Time*Sorting.Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time$Sorting.Time - expy

sqrt(sum(err^2)/nrow(delivery_time))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = delivery_time, aes(x =Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=Sorting.Time+I(Sorting.Time^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(Delivery.Time~Sorting.Time+ I(Sorting.Time*Sorting.Time) + I(Sorting.Time*Sorting.Time*Sorting.Time))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = delivery_time, aes(x = Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3), y = Delivery.Time)) + 
geom_point(color='blue') +
  geom_line(color='red',data =delivery_time, aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3), y=expy3))


predict_val <-exp(predict(reg_exp))
predict_val
rmse <-sqrt(mean((predict_val-Delivery.Time)^2))
rmse

