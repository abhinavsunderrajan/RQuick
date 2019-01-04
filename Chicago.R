#Chicago dataset has stuff with some time series in it
library(dplyr)
library(ggplot2)
library(VIM)
library("reshape2")


chicago <- readRDS("datasets/chicago")
str(chicago)
chicago <- dplyr::rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
#chicago%>%dplyr::group_by(year)%>%dplyr::summarize(num_obs=n(),max_temp=max(tmpd,na.rm = TRUE))

ggplot(chicago,aes(x=date,y=tmpd))+geom_line()

train=chicago%>%dplyr::filter(year<2005)
test=chicago%>%dplyr::filter(year==2005)

#KNN impuattion
train<-kNN(train,variable = c("tmpd"),k=3)

temp_series=ts(train$tmpd,frequency = 365,c(1987,1))



options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(temp_series)
plot(temp_series)
plot(salesDecomp)

temp_series_hw=HoltWinters(temp_series)
plot(temp_series_hw)
print(c(temp_series_hw$alpha,temp_series_hw$beta,temp_series_hw$gamma))


#now forecast for the next year
seriesforecasts2 <- forecast:::forecast.HoltWinters(temp_series_hw, h=365)
forecast:::plot.forecast(seriesforecasts2)



results=data.frame(actual=test[1:31,]$tmpd,predicted=unclass(seriesforecasts2$mean))
mean(results$predicted)
mean(results$actual)

mean(abs(results$predicted-results$actual)/results$actual)

results$date=test[1:31,]$date
results_long=melt(results,id="date")

ggplot(data=results_long,aes(x=date, y=value, colour=variable)) +
  geom_line()



