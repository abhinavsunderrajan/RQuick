library(dplyr)
library(ggplot2)
library(caret)
library("ISLR")
data("Hitters")
str(Hitters)
library(glmnet)

set.seed(3456)

#na's per column
na_count <-sapply(Hitters, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
hitters=Hitters[complete.cases(Hitters),]

#skewed dataset
hist(hitters$Salary)
hitters$Salary=log(hitters$Salary)

#I do not want to scale the salary since I am predicting it
hitters_processed=preProcess(hitters[-19],c("scale","center"))
hitters=predict(hitters_processed,hitters)

#create train test set
index <- createDataPartition(hitters$Salary, p=0.85, list=FALSE)
trainSet <- hitters[ index,]
testSet <- hitters[-index,]



control <- trainControl(method="repeatedcv", number=10, repeats=3)

#intercept sucks so remove 0 here indicates no intercept
modelFit_lm <- train(Salary~0+.,data=trainSet, 
                  method = "lm", metric="RMSE",trControl = control)
#plot(modelFit_lm$finalModel)
summary(modelFit_lm$finalModel)

pred_lm <- predict(modelFit_lm, testSet,na.action = na.pass)


#lets try ridge regression and lasso
#If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit.
#https://stats.stackexchange.com/questions/299653/caret-glmnet-vs-cv-glmnet

grid =10^ seq (10,-2, length =100)
tunegrid <- expand.grid(alpha = 0,lambda=grid)
head(tunegrid)
modelFit_ridge <- train(Salary~.,data=trainSet, tuneGrid=tunegrid,
                  method = "glmnet", metric="RMSE",trControl = control)

modelFit_ridge$bestTune

coef(modelFit_ridge$finalModel, modelFit_ridge$bestTune$lambda)

pred_ridge <- predict(modelFit_ridge, testSet,na.action = na.pass)


#lasso
tunegrid <- expand.grid(alpha = 1,lambda=grid)
head(tunegrid)
modelFit_lasso <- train(Salary~.,data=trainSet, tuneGrid=tunegrid,
                  method = "glmnet", metric="RMSE",trControl = control)
modelFit_lasso$bestTune

#you can actually see several coefficients are 0
coef(modelFit_lasso$finalModel, modelFit_lasso$bestTune$lambda)

pred_lasso <- predict(modelFit_lasso, testSet,na.action = na.pass)


#finally trying random forest regression since I havent done that before in R
tunegrid <- expand.grid(.mtry=c(2:18),.splitrule = "variance",
                        .min.node.size = c(2, 10))
control <- trainControl(method="repeatedcv", number=10, repeats=3)
dmy <- dummyVars(" ~ .", data = trainSet,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = trainSet))

model_rf <- train(Salary~., data=train_transformed, method="ranger", metric="RMSE", tuneGrid=tunegrid, trControl=control)

dmy <- dummyVars(" ~ .", data = testSet,fullRank = T)
test_transformed <- data.frame(predict(dmy, newdata = testSet))
pred_random_forest <- predict(model_rf, test_transformed,na.action = na.pass)






#final results
results=data.frame(actual=testSet$Salary,lm=pred_lm,ridge=pred_ridge,
                   lasso=pred_lasso,random_forest=pred_random_forest)
results=exp(results)

mean(abs(results$actual-results$lm)/results$actual)
mean(abs(results$actual-results$ridge)/results$actual)
mean(abs(results$actual-results$lasso)/results$actual)
mean(abs(results$actual-results$random_forest)/results$actual)


#in r negative indexing is a bit differerent
m=c(1,2,3,4,5,6)
#all except the negative indexed one
m[-2]
