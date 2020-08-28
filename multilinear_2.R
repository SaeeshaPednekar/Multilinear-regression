computer<-read.csv("F:/Excelr/Assignments/dataset/multiple linear regression/Computer_Data.csv")
View(computer)
summary(computer)
cor(computer)

attach(computer)


library(plyr)
computer$cd<-revalue(computer$cd,c("no"="0","yes"="1"))
computer$multi<-revalue(computer$multi,c("no"="0","yes"="1"))
computer$premium<-revalue(computer$premium,c("no"="0","yes"="1"))
attach(computer)
computer<-cbind( price   ,speed,hd,ram,screen,cd,multi,premium,ads,trend)
computer<-as.data.frame(computer)
#exploratory data analysis
mean(price)
mean(speed)
mean(hd)
mean(ram)
mean(screen)
mean(cd)
mean(multi)
mean(premium)
mean(ads)
mean(trend)




median(price)
median(speed)
median(hd)
median(ram)
median(screen)
median(cd)
median(multi)
median(premium)
median(ads)
median(trend)

getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(price)
getmode(speed)
getmode(hd)
getmode(ram)
getmode(screen)
getmode(cd)
getmode(multi)
getmode(premium)
getmode(ads)
getmode(trend)



var(price)
var(speed)
var(hd)
var(ram)
var(screen)
var(cd)
var(multi)
var(premium)
var(ads)
var(trend)

(price)
(speed)
(hd)
(ram)
(screen)
(cd)
(multi)
(premium)
(ads)
(trend)


sd(price)
sd(speed)
sd(hd)
sd(ram)
sd(screen)
sd(cd)
sd(multi)
sd(premium)
sd(ads)
sd(trend)

library(moments)


skewness(price)
skewness(speed)
skewness(hd)
skewness(ram)
skewness(screen)
skewness(cd)
skewness(multi)
skewness(premium)
skewness(ads)
skewness(trend)



kurtosis(price)
kurtosis(speed)
kurtosis(hd)
kurtosis(ram)
kurtosis(screen)
kurtosis(cd)
kurtosis(multi)
kurtosis(premium)
kurtosis(ads)
kurtosis(trend)


hist(price)
hist(speed)
hist(hd)
hist(ram)
hist(screen)
hist(cd)
hist(multi)
hist(premium)
hist(ads)
hist(trend)




boxplot(price,horizontal = T)
boxplot(speed,horizontal = T)
boxplot(hd,horizontal = T)
boxplot(ram,horizontal = T)
boxplot(screen,horizontal = T)
boxplot(cd,horizontal = T)
boxplot(multi,horizontal = T)
boxplot(premium,horizontal = T)
boxplot(ads,horizontal = T)
boxplot(trend,horizontal = T)



library(ggplot2)

pairs(computer)
cor(computer)

plot(price,speed)
cor(price,speed)
plot(price ,hd)
cor(price ,hd)
plot(price,ram)
cor(price,ram)
plot(price,screen)
cor(price,screen)
plot(price,cd)
cor(price,cd)
plot(price,multi)
cor(price,multi)
plot(price,premium)
cor(price,premium)
plot(price,ads)
cor(price,ads)
plot(price,trend)
cor(price,trend)


library(GGally)
windows()
ggpairs(computer)

#to check pure corelation between variables
library(corpcor)
cor(computer)
cor2pcor(cor(computer))

#models
library(car)

#model1
computer1<-lm(price~.,data=computer)
summary(computer1)
vif(computer1)
avPlots(computer1)
sqrt(mean(computer1$residuals**2))

influence.measures(computer1)
influenceIndexPlot(computer1,id.n=3)
influencePlot(computer1,id.n=3)
#model2
computer2<-lm(price~.,data=computer[-1441,])
summary(computer2)
vif(computer2)   #variance inflation factor
avPlots(computer2)
sqrt(mean(computer2$residuals**2))#RMSE

influence.measures(computer2)
influenceIndexPlot(computer2,id.n=3)
influencePlot(computer2,id.n=3)

#model3
computer3<-lm(price~.,data=computer[-c(1441,1701),])
summary(computer3)
vif(computer3)
avPlots(computer3)
sqrt(mean(computer3$residuals**2))

influence.measures(computer3)
influenceIndexPlot(computer3,id.n=3)
influencePlot(computer3,id.n=3)


#finalmodel

finalmodel<-lm(price~.,data=computer)
summary(finalmodel)


#R2 value is 0.7756