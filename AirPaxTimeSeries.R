data(AirPassengers)
library(dplyr)
library(ggplot2)
library(forecast)


plot.ts(AirPassengers)
#should take a log since it seems that the amplitude is increasing over time
AirPassengers=log(AirPassengers)
plot.ts(AirPassengers)

abline(reg=lm(AirPassengers~time(AirPassengers)), col="red")
air_pax_decompose=decompose(AirPassengers)
plot(air_pax_decompose)

air_pax_hw=HoltWinters(AirPassengers)
plot(air_pax_hw)
print(c(air_pax_hw$alpha,air_pax_hw$beta,air_pax_hw$gamma))

#now forecast for the next year i.e 1961
seriesforecasts2 <- forecast:::forecast.HoltWinters(air_pax_hw, h=12)
forecast:::plot.forecast(seriesforecasts2)

