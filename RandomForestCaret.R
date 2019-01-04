#https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
library(caret)
library("RANN")




#read the data
train<-read.csv("datasets/train_u6lujuX_CVtuZ9i.csv",stringsAsFactors = T)

#na's per column
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)



#just showing a way to deal with stuff
cor(train$ApplicantIncome,train$CoapplicantIncome,use = "complete.obs")

unique(train$Loan_Amount_Term)

preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))
train_processed <- predict(preProcValues, train)
sum(is.na(train_processed))

unique(train$Loan_Amount_Term)

train$Loan_Amount_Term=as.factor(train$Loan_Amount_Term)




#remove id columsn whats the point int having it
id<-train_processed$Loan_ID
train_processed$Loan_ID<-NULL


dmy <- dummyVars(" ~ . -Loan_Status", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
train_transformed$Loan_Status=train_processed$Loan_Status
str(train_transformed)



#Here, “fullrank=T” will create only (n-1) columns for a categorical column with n different levels.
#This works well particularly for the representing categorical predictors like gender, married, etc. where we only 
#have two levels: Male/Female, Yes/No, etc. because 0 can be used to represent one class while 1 represents the other class in same column.


index <- createDataPartition(train_transformed$Loan_Status, p=0.8, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]


control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Train and Tune the RF
modelFit <- train(Loan_Status~.,data=trainSet, method = "rf", metric="Accuracy",trControl = control)



#prob <- predict(modelFit, dataTest,na.action = na.pass,type = "prob")
pred <- predict(modelFit, testSet,na.action = na.pass)
cm <- confusionMatrix(pred, testSet$Loan_Status)
cm
plot(varImp(modelFit))


#now let us try tuning the mtry parameter
#mrty is the number of features used inthe random forest for each of the trees
print(modelFit$finalModel$mtry)
print(modelFit$finalModel$ntree)


#letrs try some grid serach here and see if things velangify

set.seed(42)
tunegrid <- expand.grid(.mtry=c(2:20))
rf_gridsearch <- train(Loan_Status~., data=trainSet, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)

plot(rf_gridsearch)

#now rf grid serach it self decieded that the best mtry is 12 and gives you that as the final model
print(rf_gridsearch$finalModel$mtry)

pred <- predict(rf_gridsearch, testSet,na.action = na.pass)
cm <- confusionMatrix(pred, testSet$Loan_Status)
cm


# Using caret with the default grid to optimize tune parameters automatically
# GBM Tuning parameters:
# n.trees (# Boosting Iterations)
# interaction.depth (Max Tree Depth)
# shrinkage (Shrinkage)
# n.minobsinnode (Min. Terminal Node Size)

Grid <- expand.grid( n.trees = seq(50,200,50),interaction.depth=seq(1,3),shrinkage = c(0.1),n.minobsinnode=10)
gbm_model=train(Loan_Status~., data=trainSet, method="gbm", metric="Accuracy", 
                tuneGrid=Grid, trControl=control)

pred <- predict(gbm_model, testSet,na.action = na.pass)
cm <- confusionMatrix(pred, testSet$Loan_Status)
cm


gbm_model$finalModel


