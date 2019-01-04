library(caTools)
library(ROCR)
set.seed(88)
quality<-read.csv("quality.csv")
quality_split<-sample.split(quality$PoorCare,SplitRatio=0.75)
quality_train<-subset(quality,quality_split==TRUE)
quality_test<-subset(quality,quality_split==FALSE)

model_quality<-glm(PoorCare~OfficeVisits+Narcotics,data=quality_train,family=binomial)
predictTrain=predict(model_quality,type="response")
summary(predictTrain)
tapply(predictTrain,quality_train$PoorCare,mean)
table(quality_train$PoorCare,predictTrain>0.2)


#New model
ROCRPred=prediction(predictTrain,quality_train$PoorCare)
ROCRPerf=performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize=TRUE)
predictTest=predict(model_quality,type="response",newdata=quality_test)
tapply(predictTest,quality_test$PoorCare,mean)
table(quality_test$PoorCare,predictTest>0.3)
ROCRpredTest = prediction(predictTest, quality_test$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
