#ml bench has some neat datasets you can use for such demo stuff
library(mlbench)
library(dplyr)
library(ggplot2)
#for pairplots
library(corrplot)


set.seed(123)
data(BostonHousing)
summary(BostonHousing)


train <- BostonHousing %>% dplyr::sample_frac(.8)
test  <- dplyr::anti_join(BostonHousing, train)


pairs(train[,-4])
M <- cor(train[,-4])
corrplot(M, method = "circle")

model=lm("medv~lstat+age+rm*lstat+chas -1",data=train[c(-369,-372,-373),])
summary(model)
#plot(model)

test$medv_pred=predict(model,test)
rmse<-sqrt(mean((test$medv - test$medv_pred)^2))
mape<-mean(abs((test$medv-test$medv_pred)/test$medv))
print(mape)

p <-test %>% ggplot(aes(medv,medv_pred)) +
  geom_point(alpha=0.5) + stat_smooth(method="lm",col="red") +
  xlab('Actual value of medv') +ylab('Predicted value of medv')+theme_bw()

p