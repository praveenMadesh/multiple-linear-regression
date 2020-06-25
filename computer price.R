### In the given data we have to predict the price of computer by using all column
# so Y <- price and x <- all column
library(readr)
c_p<-read.csv(file.choose())
View(c_p)
c_p<-c_p[,-1]
c_p<-c_p[,-6]
c_p<-c_p[,-6]
c_p<-c_p[,-6]
View(c_p)
summary(c_p)
pairs(c_p)
cor(c_p)

library(corpcor)
cor2pcor(cor(c_p))
###
model.cp<-lm(price~.,data=c_p)
summary(model.cp)## p values is less then 0.05 and R^2 values is 0.71
pred <- predict(model.cp)
pred
avplot(model.cp,id.n=2,id.cex=0.7)
plot(lm(price~.,data = c_p))
summary(lm(price~.,data=c_p))
attach(c_p)
boxplot(pred,horizontal = T)
hist(pred)##data are normally distributed
