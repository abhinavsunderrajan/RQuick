library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(corrplot)
library(caret)
library(ROCR)
library(MASS)
library(stats4)
library (boot)


trainBikeSharing<-read.csv("bike-sharing/train.csv")
trainBikeSharing$season=as.factor(trainBikeSharing$season)

trainBikeSharing$is_weekend=ifelse(trainBikeSharing$workingday==0 & trainBikeSharing$holiday==0,1,0)

trainBikeSharing$holiday=as.factor(trainBikeSharing$holiday)
trainBikeSharing$is_weekend=as.factor(trainBikeSharing$is_weekend)
trainBikeSharing$workingday=as.factor(trainBikeSharing$workingday)
trainBikeSharing$weather=as.factor(trainBikeSharing$weather)
trainBikeSharing$datetime=strptime(trainBikeSharing$datetime, "%Y-%m-%d %H:%M:%S")
trainBikeSharing$hour_of_day=trainBikeSharing$datetime$hour
trainBikeSharing$hour_of_day=as.factor(trainBikeSharing$hour_of_day)
trainBikeSharing$month_of_year=as.factor(trainBikeSharing$datetime$mon)
trainBikeSharing$year=as.factor(trainBikeSharing$datetime$year)

trainBikeSharing<-dplyr::select(trainBikeSharing,-datetime)


M <- cor(dplyr::select(trainBikeSharing,c(temp,atemp,humidity,windspeed,count)))
corrplot(M, method="circle")

train_holidays<-dplyr::filter(trainBikeSharing,workingday==0)
p1 <- ggplot(train_holidays, aes(hour_of_day,registered))
p1 + geom_boxplot()

train_working<-dplyr::filter(trainBikeSharing,workingday==1)
p2 <- ggplot(train_working, aes(hour_of_day,registered))
p2 + geom_boxplot()

p2 <- ggplot(trainBikeSharing, aes(season,count))
p2 + geom_boxplot()

p2 <- ggplot(trainBikeSharing, aes(humidity*atemp,count))
p2 + geom_line()


tcontrol <- trainControl(method = "cv", number = 10)
model_registered<-train(log(registered+1)~workingday+season+poly(atemp,3)+poly(humidity,2)+
                      atemp:windspeed+atemp:humidity+hour_of_day,na.action = na.pass,
                         data=trainBikeSharing, method = "lm", metric = "RMSE", trControl = tcontrol)

model_casual<-train(casual~workingday+season+poly(atemp,3)+poly(humidity,2)+
                          atemp:windspeed+atemp:humidity+hour_of_day,na.action = na.pass,
                        data=trainBikeSharing, method = "lm", metric = "RMSE", trControl = tcontrol)


