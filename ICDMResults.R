library(dplyr)
library(ggplot2)
require(gridExtra)
library(reshape2)

#see this link for dplyr do http://www.milanor.net/blog/dplyr-do-tips-for-using-and-programming/

xgboost_city<-read.csv("results_6.csv")
rsp_city<-read.csv("speed_profiles_6.csv")

y_true=xgboost_city$label
y_pred=xgboost_city$predicted
ape_ett=abs(y_true-y_pred)/y_true
mean(ape_ett,na.rm=TRUE)

#transform xgboost results
xgboost_city$MAPE=abs(xgboost_city$predicted-xgboost_city$label)*100/xgboost_city$label
xgboost_city$minute_bucket <- cut(xgboost_city$minute_of_day,include.lowest = TRUE,dig.lab=10, breaks = seq(0, 1440, by = 10))
xgboost_city$driver_distance=xgboost_city$driver_distance/1000
xgboost_city=dplyr::filter(xgboost_city,driver_distance<=20.0)
xgboost_city$driver_distance <- cut(xgboost_city$driver_distance,include.lowest = TRUE,dig.lab=10, breaks = seq(0, 20, by = 4))


#transform RSP results
rsp_city$MAPE=abs(rsp_city$expected_time-rsp_city$label)*100/rsp_city$label
rsp_city$minute_bucket <- cut(rsp_city$minute_of_day,include.lowest = TRUE,dig.lab=10, breaks = seq(0, 1440, by = 10))
rsp_city=dplyr::filter(rsp_city,driver_distance<=20.0)
rsp_city$driver_distance <- cut(rsp_city$driver_distance,include.lowest = TRUE,dig.lab=10, breaks = seq(0, 20, by = 4))

#by time of the day
xgb_mape=as.data.frame(xgboost_city%>%dplyr::group_by(minute_bucket)%>%dplyr::summarise(mean_mape=mean(MAPE,na.rm = TRUE)))
rsp_mape=as.data.frame(rsp_city%>%dplyr::group_by(minute_bucket)%>%dplyr::summarise(mean_mape=mean(MAPE,na.rm = TRUE)))
rsp_mape$process="From RSP"
xgb_mape$process="Corrective GBDT Model"
xgb_mape$minute_bucket=as.factor(xgb_mape$minute_bucket)
rsp_mape$minute_bucket=as.factor(rsp_mape$minute_bucket)

result_mape=rbind(xgb_mape,rsp_mape)
result_mape$process=as.factor(result_mape$process)



p2<-ggplot(result_mape,aes(x=minute_bucket,y=mean_mape,colour=process,group=result_mape$process))+
  geom_point(size=2.5)+geom_line(size=1.2)+theme_bw()+labs(color='Process',x="Time of the day between minutes 0-1339",y="MAPE")+
  #scale_y_continuous(limits = c(12,23))+
  theme(legend.position="bottom",axis.text.x = element_blank(),axis.title.x = element_text(size=18),
        axis.text.y = element_text(size=14, vjust = 1),axis.ticks.x = element_blank(),axis.title.y = element_text(size=18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18))
p2

#ggsave("ETT-TOD-SG.pdf",p2,path = "/Users/abhinav.sunderrajan/Desktop",width = 10, height = 6, units = "in")


#by distance
xgb_mape=as.data.frame(xgboost_city%>%dplyr::group_by(driver_distance)%>%dplyr::summarise(mean_mape=mean(MAPE,na.rm = TRUE)))
rsp_mape=as.data.frame(rsp_city%>%dplyr::group_by(driver_distance)%>%dplyr::summarise(mean_mape=mean(MAPE,na.rm = TRUE)))

rsp_mape$process="From RSP"
xgb_mape$process="Corrective GBDT Model"
rsp_mape$driver_distance=as.factor(rsp_mape$driver_distance)
xgb_mape$driver_distance=as.factor(xgb_mape$driver_distance)

result_mape=rbind(xgb_mape,rsp_mape)
result_mape$process=as.factor(result_mape$process)



p1<-ggplot(result_mape,aes(x=driver_distance,y=mean_mape,colour=process,group=result_mape$process))+
  geom_point(size=2.5)+geom_line(size=1.2)+theme_bw()+labs(color='Process',x="Distance buckets",y="MAPE")+
  theme(legend.position="bottom", axis.text.x = element_text(size=14,angle = 45, hjust = 1),axis.title.x = element_text(size=18),
        axis.text.y = element_text(size=14, vjust = 1),axis.title.y = element_text(size=18),
        legend.text = element_text(size = 18),legend.title = element_text(size = 18))
p1

#ggsave("ETA-Distance.pdf",p1,path = "/Users/abhinav.sunderrajan/Desktop",width = 10, height = 6, units = "in")
