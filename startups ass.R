### In the given data we have to predict the profit by using all column
# so Y <- profit and x <- all columns (multi linear regression)
# Data type: allcolumns are continuous
library(readr)
startups<-read.csv(file.choose())
View(startups)
startups<-startups[,-4]  ###removing of 4th column that is state column
View(startups)
pairs(startups)
cor(startups)##correlation coefficient between R.D.speed and profit is 0.9729 that mean normally distributed
## administration and marketing.spend is -0.032 -vecorrelation coefficient  

library(corpcor)
cor2pcor(cor(startups))

###########
model.startups <- lm(Profit ~.,data=startups)
summary(model.startups)
#####
model.startupsA<-lm(Profit~Administration,data=startups)
summary(model.startupsA)
####
model.startupsM<-lm(Profit~Marketing.Spend,data = startups)
summary(model.startupsM)### R^2 value is good but p value is greater then 0.05 
#in adiministration and marketing spend
#### so we using Variance Inflation Factors
vif(model.startups)
avPlots(model.startups,id.n=2,id.cex=0.7)
influenceIndexPlot(model.startupsA,id.n=3)
influencePlot(model.startupsA,id.n=3)

model.startups1<-lm(Profit~.-Administration,data=startups[-c(49,47,50),])
summary(model.startups1) ##after ramoving of vif the data are significent
plot(model.startups1)##now data are normaliy distributed 
hist(residuals(model.startups1))

