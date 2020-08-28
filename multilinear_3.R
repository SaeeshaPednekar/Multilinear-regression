corolla<-read.csv("F:/Excelr/Assignments/dataset/multiple linear regression/ToyotaCorolla.csv")
View(corolla)
corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(corolla)
attach(corolla)
str(corolla)
corolla<-as.data.frame(corolla)
sum(is.na(corolla))


#exploratory data analysis

mean(Price)
mean(Age_08_04)
mean(KM)
mean(HP)
mean(cc)
mean(Doors)
mean(Gears)
mean(Quarterly_Tax)
mean(Weight)


median(Price)
median(Age_08_04)
median(KM)
median(HP)
median(cc)
median(Doors)
median(Gears)
median(Quarterly_Tax)
median(Weight)


getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(Price)
getmode(Age_08_04)
getmode(KM)
getmode(HP)
getmode(cc)
getmode(Doors)
getmode(Gears)
getmode(Quarterly_Tax)
getmode(Weight)



var(Price)
var(Age_08_04)
var(KM)
var(HP)
var(cc)
var(Doors)
var(Gears)
var(Quarterly_Tax)
var(Weight)


sd(Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
sd(Doors)
sd(Gears)
sd(Quarterly_Tax)
sd(Weight)

library(moments)
skewness(Price)
skewness(Age_08_04)
skewness(KM)
skewness(HP)
skewness(cc)
skewness(Doors)
skewness(Gears)
skewness(Quarterly_Tax)
skewness(Weight)


kurtosis(Price)
kurtosis(Age_08_04)
kurtosis(KM)
kurtosis(HP)
kurtosis(cc)
kurtosis(Doors)
kurtosis(Gears)
kurtosis(Quarterly_Tax)
kurtosis(Weight)


hist(Price)
hist(Age_08_04)
hist(KM)
hist(HP)
hist(cc)
hist(Doors)
hist(Gears)
hist(Quarterly_Tax)
hist(Weight)

boxplot(Price,horizontal = T)
boxplot(Age_08_04,horizontal = T)
boxplot(KM,horizontal = T)
boxplot(HP,horizontal = T)
boxplot(cc,horizontal = T)
boxplot(Doors,horizontal = T)
boxplot(Gears,horizontal = T)
boxplot(Quarterly_Tax,horizontal = T)
boxplot(Weight,horizontal = T)

library(ggplot2)
cor(corolla)
pairs(corolla)


#to check pure corelation between variables
library(corpcor)
cor(corolla)
cor2pcor(cor(corolla))

#correlation and plotting
plot(Price,Age_08_04)
cor(Price,Age_08_04)

plot(Price,KM)
cor(Price,KM)

plot(Price,HP)
cor(Price,HP)

plot(Price,cc)
cor(Price,cc)

plot(Price,Doors)
cor(Price,Doors)

plot(Price,Gears)
cor(Price,Gears)

plot(Price,Quarterly_Tax)
cor(Price,Quarterly_Tax)

plot(Price,Weight)
cor(Price,Weight)


#Building models
library(car)
#model1
corolla1<-lm(Price~.,data = corolla)
summary(corolla1)
vif(corolla1)
avPlots(corolla1)
sqrt(mean(corolla1$residuals**2))

influence.measures(corolla1)
influenceIndexPlot(corolla1,id.n=3)
influencePlot(corolla1,id.n=3)

#model2
corolla2<-lm(Price~.,data = corolla[-81,])
summary(corolla2)
sqrt(mean(corolla2$residuals**2))
#model3
corolla3<-lm(Price~Age_08_04+KM+HP+log(cc)+log(Doors)+Gears+Quarterly_Tax+Weight,data = corolla[-81,])
summary(corolla3)
sqrt(mean(corolla3$residuals**2))


#model4
corolla4<-lm(Price~Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = corolla[-81,])
summary(corolla4)
vif(corolla4)
avPlots(corolla4)
sqrt(mean(corolla4$residuals**2))

influence.measures(corolla4)
influenceIndexPlot(corolla4,id.n=3)
influencePlot(corolla4,id.n=3)



#finalmodel
finalmodel<-lm(Price~Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = corolla[-81,])
summary(finalmodel)

# R-squared:  0.8702 .the final model is the best model
