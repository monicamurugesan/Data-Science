emp_data <-read.csv(file.choose())
View(emp_data)
emp_data
summary(emp_data)
attach(emp_data)
plot(Churn_out_rate~Salary_hike)
cor(Churn_out_rate,Salary_hike)
sd(Churn_out_rate)
var(Churn_out_rate)
reg <-lm(Churn_out_rate~Salary_hike)
summary(reg)
reg$residuals
plot(reg)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))
confint(reg,level=0.95)
predict(reg,interval="predict")
library(ggplot2)
ggplot(data=emp_data,aes(x=Salary_hike,y=Churn_out_rate))+geom_point(color='red')+geom_line(color='blue',data=emp_data,aes(x=Salary_hike,y=Churn_out_rate))


plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
str(emp_data)
reg_l <-lm(Churn_out_rate~log(Salary_hike))
summary(reg_l)
predict(reg_l)
reg_l$residuals
sum(sqrt(reg_l$residuals^2)/nrow(emp_data))
confint(reg_l,level=0.95)
predict(reg_l,interval="confidence")

ggplot(data=emp_data,aes(x=log(Salary_hike),y=Churn_out_rate))+geom_point(color='red')+geom_line(color='blue',data=emp_data,aes(x=log(Salary_hike),y=Churn_out_rate))


plot(log(Churn_out_rate),Salary_hike)
cor(log(Churn_out_rate),Salary_hike)
reg_exp <-lm(log(Churn_out_rate)~Salary_hike)
summary(reg_exp)
predict(reg_exp)
reg_exp$residuals
r<-predict(reg_exp)
ne <-exp(r)
error=emp_data$Salary_hike-ne
sum(sqrt(reg_exp$residuals^2)/nrow(emp_data))
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
ggplot(data=emp_data,aes(x=Salary_hike,y=log(Churn_out_rate)))+geom_point(color='red')+geom_line(color='blue',data=emp_data,aes(x=Salary_hike,y=log(Churn_out_rate)))


plot(Churn_out_rate, Salary_hike)
plot(Churn_out_rate*Churn_out_rate, Salary_hike)

cor(Churn_out_rate*Churn_out_rate, Salary_hike)

plot(Churn_out_rate*Churn_out_rate, log(Salary_hike))

cor(Churn_out_rate, log(Salary_hike))
cor(Churn_out_rate*Churn_out_rate, log(Salary_hike))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(Churn_out_rate ~ Salary_hike+ I(Salary_hike*Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp_data$Salary_hike - expy

sqrt(sum(err^2)/nrow(delivery_time))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x =Salary_hike+ I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))



predict_val <-exp(predict(reg_exp))
predict_val
rmse <-sqrt(mean((predict_val-Churn_out_rate)^2))
rmse
