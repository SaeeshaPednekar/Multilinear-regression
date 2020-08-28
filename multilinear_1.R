#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

#R&D Spend -- Research and devolop spend in the past few years
#Administration -- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years
#State -- states from which data is collected
#Profit  -- profit of each state in the past few years



startups<-read.csv("F:/Excelr/Assignments/dataset/multiple linear regression/50_Startups.csv")
View(startups)
cor(startups)
install.packages("plyr")
library(plyr)
startups$State <- revalue(startups$State, c("New York"="0", "California"="1", "Florida"="2")) 
str(startups)
attach(startups)
startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


startups <- as.data.frame(startups)

attach(startups)
summary(startups) 
cor(startups)
#exploratory data analysis

mean(RD_Spend)
mean(Administration)
mean(Marketing.Spend)
mean(Profit)
mean(State)

median(RD_Spend)
median(Administration)
median(Marketing.Spend)
median(Profit)
median(State)



getmode<-function(x){uniquv<-unique(x)
uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(RD_Spend)
getmode(State)
getmode(Administration)
getmode(Marketing.Spend)
getmode(Profit)

var(RD_Spend)
var(State)
var(Administration)
var(Marketing.Spend)
var(Profit)

sd(RD_Spend)
sd(Administration)
sd(Profit)
sd(Marketing_Spend)

range(RD_Spend)
range(Administration)
range(Marketing.Spend)
range(Profit)
library(moments)
skewness(RD_Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)
skewness(State)

kurtosis(RD_Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)

boxplot(RD_Spend)
boxplot(Administration)
boxplot(Marketing.Spend)
boxplot(Profit)
boxplot(State)

barplot(RD_Spend)
barplot(Administration)
barplot(Marketing.Spend)
barplot(Profit)
barplot(State)
library(ggplot2)

summary(startups)
hist(Profit)
skewness(Profit)
boxplot(startups$Profit,horizontal = T)


#plot
plot(R.D.Spend,Profit)
cor(R.D.Spend,Profit)
plot(Administration,Profit)
cor(Administration,Profit)
plot(Marketing.Spend,Profit)
cor(Marketing.Spend,Profit)
plot(State,Profit)
cor(State,Profit)
plot(R.D.Spend,Administration)
cor(R.D.Spend,Administration)
plot(R.D.Spend,Marketing.Spend)
cor(R.D.Spend,Marketing.Spend)
 plot(Administration,Marketing.Spend)
#scatter plots for all pairs
 pairs(startups)
#cor for all
 cor(startups)
# State<-as.character("california","Florida","New York")
 class(startups)
 #Statetonumeric<-as.numeric(State)

library(GGally)
windows()
ggpairs(startups)

#to check pure corelation between variables
library(corpcor)
cor(startups)
cor2pcor(cor(startups))
#MODELS

model.startups<-lm(Profit~.,data=startups)
summary(model.startups)

sqrt(mean(model.startups$residuals**2))

model.startupsA<-lm(Profit~Administration)
summary(model.startupsA)

model.startupsM<-lm(Profit~Marketing.Spend)
summary(model.startupsM)

model.startupsS<-lm(Profit~State)
summary(model.startupsS)
model.startupsAM<-lm(Profit~Administration+Marketing.Spend)
summary(model.startupsAM)
#model.startupsAMS<-(Profit~Administration+Marketing.Spend+State)
#summary(model.startupsAMS)

library(car)
vif(model.startups)
avPlots(model.startups)

influence.measures(model.startups)
influenceIndexPlot(model.startups,id.n=3)
influencePlot(model.startups,id.n=3)

#model1
model.startups1<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=startups)
summary(model.startups1)
#model2
model.startups2<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+State,data=startups[-50,])
summary(model.startups2)
vif(model.startups1) #variance inflation factor
mean(model.startups1$residuals)
hist(model.startups1$residuals)
sqrt(mean(model.startups1$residuals**2))  #RMSE


#model.startups2
model.startups2<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+sqrt(State),data=startups)
summary(model.startups2)
#model.startups3
model.startups3<-lm(Profit~RD_Spend+Marketing_Spend,data=startups[-c(49,50),])
summary(model.startups3)
vif(model.startups3)
sqrt(mean(model.startups3$residuals**2))
avPlots(model.startups3)
mean(model.startups3$residuals)
hist(model.startups3$residuals)
skewness(model.startups3$residuals)
plot(model.startups3)
#final model 

FinalModel<-lm(Profit~RD_Spend+Marketing_Spend,data=startups[-c(49,50),])
summary(FinalModel)
sqrt(mean(FinalModel$residuals**2))

#R-squared:  0.9609.

