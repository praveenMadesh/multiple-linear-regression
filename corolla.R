#consider only the below columns and prepare a prediction model for predicting Price.
### In the given data we have to predict the pricec by using all column
# so Y <- price and x <- Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight" (multi linear regression)
# Data type: all columns are continuous ,,gears  and doors are discrete
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

library(readr)
corolla <- read.csv(file.choose())
View(corolla)
summary(corolla)
attach(corolla)
corolla <- corolla[c("Price","Age_08_04","KM","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
summary(corolla)
pairs(corolla)
cor(corolla)

library(corpcor)
cor2pcor(cor(corolla))

####linear model
model_corolla <- lm(Price~.,data = corolla)###lm(y~all columns)
summary(model_corolla)## in column cc,doors,gears,quarterly_tax the p value is greater then 0.05

#### model based on cc
model_cc <- lm(Price~cc,data = corolla)
summary(model_cc)

##### model based on Doors
model_Doors <- lm(Price~Doors,data = corolla)
summary(model_Doors)

##### model based on Q
model_Q <- lm(Price~Quarterly_Tax)
summary(model_Q)

##### model based on cc+Doors
model_4 <- lm(Price~cc+Doors,data = corolla)
summary(model_4)

##### model based on cc+Q
model_3 <- lm(Price~cc+Quarterly_Tax)
summary(model_3)

####model based on Doors+Q
model_2 <- lm(Price~Doors+Quarterly_Tax)
summary(model_2)
### model based on cc+Doors+Q
model_1 <- lm(Price~cc+Doors+Quarterly_Tax,data = corolla)
summary(model_1)

library(ggplot2)
vif(model_corolla)
avplots(model_corolla,id.n=3,id.cex=0.7)
library(car)
influence.measures(model_corolla)
influenceIndexPlot(model_corolla,id.n=3)
influencePlot(model_corolla,id.n=3)

model <- lm(Price~.,data=corolla[-c(81,222,961),])
summary(model)##after removing of all vif the data are significent
plot(model)##normal Q-Q plot data are normally distributed
hist(residuals(model))

