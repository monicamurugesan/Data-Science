sal_hike <-read.csv(file.choose())
View(sal_hike)
summary(sal_hike)
mean(sal_hike$Salary)
var(sal_hike$Salary)
sd(sal_hike$Salary)
attach(sal_hike)
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)
regsal <-lm(Salary~YearsExperience)
summary(regsal)
sum(sqrt(regsal$residuals^2)/nrow(sal_hike))
predict(regsal)
confint(regsal,level=0.95)
predict(regsal,interval="predict")
library(ggplot2)
ggplot(data=sal_hike,aes(x=YearsExperience,y=Salary))+geom_point(color="red")+geom_line(color="blue",data=sal_hike,aes(x=YearsExperience,y=Salary))


plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
regsal_exp <-lm(log(YearsExperience)~Salary)
summary(regsal_exp)
sum(sqrt(regsal_exp$residuals^2)/nrow(sal_hike))
predict(regsal_exp)
exp <-predict(regsal_exp)
err <-exp(exp)
error<-sal_hike$YearsExperience-err
confint(regsal_exp,level=0.95)



#log
plot(log(Salary),YearsExperience)
cor(log(Salary),YearsExperience)
reg_log <-lm(Salary~log(YearsExperience))
summary(reg_log)
sum(sqrt(reg_log$residuals^2)/nrow(sal_hike))
predict(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

#Quadratic
plot(Salary,YearsExperience)
plot(Salary*Salary,YearsExperience)
cor(Salary,YearsExperience)
cor(Salary*Salary,YearsExperience)
plot(Salary*Salary,log(YearsExperience))
cor(Salary*Salary,log(YearsExperience))
plot(Salary,log(YearsExperience))
cor(Salary,log(YearsExperience))

regquad<-lm(Salary~YearsExperience+I(YearsExperience*YearsExperience))
summary(regquad)
sum(sqrt(regquad$residual^2)/nrow(sal_hike))
confint(regquad,level=0.95)
logplot <-predict(regquad)
regqx <-exp(logplot)
predict(regquad,interval = "confidence")
predict(regquad,interval="predict")
ggplot(data=sal_hike,aes(x=YearsExperience+I(YearsExperience*YearsExperience),y=Salary))+geom_point(color="red")+geom_line(data=sal_hike,color="blue",aes(x=YearsExperience+I(YearsExperience*YearsExperience),y=logplot))

err=sal_hike$YearsExperience-regqx


plot(YearsExperience,Salary)
plot(YearsExperience,log(Salary))
plot(YearsExperience*YearsExperience,Salary)
plot(YearsExperience*YearsExperience,log(Salary))
plot(YearsExperience*YearsExperience*YearsExperience,Salary)
cor(YearsExperience,Salary)
cor(YearsExperience*YearsExperience,Salary)
cor(YearsExperience,log(Salary))
cor(YearsExperience*YearsExperience,log(Salary))
cor(YearsExperience*YearsExperience*YearsExperience,Salary)
cor(YearsExperience*YearsExperience*YearsExperience,log(Salary))

regpl <-lm(Salary~YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience))
summary(regpl)
predict(regpl)
confint(regpl,level=0.95)
sum(sqrt(regpl$residuals^2)/nrow(sal_hike))
lp <-predict(regpl)
pl <-exp(lp)
error <-regpl$residuals-pl
ggplot(data=sal_hike,aes(x=YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience),y=Salary))+geom_point(color="red")+geom_line(data=sal_hike,color="blue",aes(x=YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience),y=pl))


rmse <-sqrt(mean((regqx-Salary)^2))
rmse
