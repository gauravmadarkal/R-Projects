homedata = read.csv("C:\\Users\\Sathishkumar.k\\Documents\\gauravProjects\\Quad_Datasets\\gaurav_data.csv")
homedata_renew=homedata[,-c(15)]
homedata_renew=na.omit(homedata_renew)
colnames(homedata_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
homedata_renew=subset(homedata_renew,Eact>=0)
homedata_renew$event_time= strptime(homedata_renew$event_time,"%Y-%m-%d %H:%M:%OS")

homedata_renew$event_time=as.POSIXct(homedata_renew$event_time)

homedata_renew$date= as.POSIXct(homedata_renew$event_time,format='%Y-%m-%d',tz= "UTC")
homedata_renew = homedata_renew[order(homedata_renew$date,decreasing = FALSE),]
homedata_renew$EveryDate = as.Date(homedata_renew$date)
library(dplyr)
homedata_renew = homedata_renew %>% 
  mutate(ID = group_indices_(homedata_renew, .dots=c("EveryDate")))
homedata_renew$time = as.ITime(homedata_renew$event_time)
homedata_renew$time = chron(times = homedata_renew$time)

record_count = summarise(group_by(homedata_renew,homedata_renew$EveryDate),count=n())

library(ggplot2)
ggplot(homedata_renew,aes(x=homedata_renew$event_time,y=homedata_renew$V))
homedata_renew$voltage_category = ifelse(homedata_renew$V > 250 | homedata_renew$V < 200,"DANGER","NODANGER")
g1 <- ggplot(homedata_renew, aes(homedata_renew$event_time,color = homedata_renew$voltage_category))
g1 <- g1 + geom_point(stat = "identity",aes(y=homedata_renew$V))
g1 + labs(x = "EVENT TIME",y="VOLTAGE",title = "VOLTAGE VARIATION PLOT")
g1+ scale_fill_manual(values = alpha(c("blue", "red"), .3))


#homedata_renew$time = format(as.POSIXct(strptime(homedata_renew$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
time10am = "10:00:00";
time7pm = "19:00:00";
time7pm = chron(times = time7pm)
time10am = chron(times = time10am)

homedata_renew$switch = ifelse(homedata_renew$time > time10am & homedata_renew$time < time7pm,1, 0)
g1 <- ggplot(homedata_renew, aes(homedata_renew$event_time))
g1 <- g1 + geom_line(aes(y=homedata_renew$switch))
g1 + labs(x = "days",y="voltage variation",title = "daysname vs voltage variation")

power_ts=ts(homedata_renew$switch,start=min(homedata_renew$event_time),frequency=1440)
plot.ts(power_ts)

power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)

power_ts_hw=HoltWinters(power_ts, gamma = FALSE)#Holtwinters 
plot(power_ts_hw)
library(forecast)
#forecast

power_ts_hw_fcst=forecast:::forecast.HoltWinters(power_ts_hw,n.ahead=1)
forecast:::plot.forecast(power_ts_hw_fcst)

arimaModel <- auto.arima(power_ts)
powerforecast <- forecast(arimaModel, h=5)
library(plyr)
count_days <- count(homedata_renew,ID)
