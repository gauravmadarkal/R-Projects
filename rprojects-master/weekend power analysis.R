library(caret)

library(dplyr)

library(ggplot2)
library(ggpubr)

daysData <- function(dataset,dayname){
  
  dataset$daynames = format(dataset$date,"%a")
  print(head(dataset$daynames))
  switch (dayname,
          MON = {
            dataset = with(dataset,dataset[(daynames == "Mon"),])
            
          },
          TUE ={
            dataset = with(dataset,dataset[(daynames == "Tue"),])
          },
          WED ={
            dataset = with(dataset,dataset[(daynames == "Wed"),])
            
          },
          THU = {
            dataset = with(dataset,dataset[(daynames == "Thu"),])
          },
          FRI = {
            dataset = with(dataset,dataset[(daynames == "Fri"),])
          },
          SAT = {
            dataset = with(dataset,dataset[(daynames == "Sat"),])
          },
          SUN = {
            dataset = with(dataset,dataset[(daynames == "Sun"),])
          },
          WEEKDAY ={
            dataset = dataset[c(which(dataset$daynames %in% c('Mon','Tue','Wed','Thu','Fri'))),]
            
          },
          WEEKEND = {
            dataset = dataset[c(which(dataset$daynames %in% c('Sat','Sun'))),]
          }
  )
  return(dataset)
} 
energydataset1=read.csv("C:\\Users\\SakshamA\\Documents\\Girish_Quad\\Quad_Datasets\\gaurav_data.csv")
energydataset1_renew=energydataset1[,-c(15)]
energydataset1_renew=na.omit(energydataset1_renew)
colnames(energydataset1_renew)= c("mid","Device","V","I","F","Eact","Ereac","Eapp","event_time","log_time","switch","alarm","v","ip")
energydataset1_renew=subset(energydataset1_renew,Eact>=0)
energydataset1_renew$event_time= strptime(energydataset1_renew$event_time,"%Y-%m-%d %H:%M:%OS")
#to print only date then use ,as.Date(energydataset1_renew$event_time, "%Y-%m-%d")
plot(energydataset1_renew$event_time, energydataset1_renew$switch,pch = 16,cex.axis = 0.5,col = "blue")
#lines(energydataset1_renew$event_time,energydataset1_renew$switch , col = "steelblue")
energydataset1_renew$event_time=as.POSIXct(energydataset1_renew$event_time)

energydataset1_renew$date= as.POSIXct(energydataset1_renew$event_time,format='%Y-%m-%d',tz= "UTC")

energydataset1_renew$daynames=format(energydataset1_renew$date,"%a")
energydataset1_renew=energydataset1_renew[ order(energydataset1_renew$event_time , decreasing = FALSE ),]

weekend = daysData(energydataset1_renew,"WEEKEND")


ggplot(data =daily_renew, aes(x =daily_renew$daily_dates, y =daily_renew$daily_power_usage)) + geom_bar(color = as.Date(daily_renew$daily_dates),stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = daily_renew$daily_dates)

library(xts)
data <- as.xts(weekend$Eact,order.by=as.Date(weekend$event_time))
weekendxts <- apply.daily(data,sum)
weekend=as.data.frame(index(weekendxts))
weekendxts=as.data.frame(weekendxts)
weekend$weekend_day_power_usage=weekendxts[1:nrow(weekendxts),]
colnames(weekend)=c("weekend_dates","weekend_day_power_usage")
rm(weekendxts)
weekend$daynames=format(weekend$weekend_dates,"%a")


weekend=weekend[weekend$daynames!="Fri", ]

weekend$weekend_dates= strptime(weekend$weekend_dates,"%Y-%m-%d")
weekend$weekend_dates=as.POSIXct(weekend$weekend_dates)



power_ts=ts(weekend$weekend_day_power_usage,start=min(weekend$weekend_dates),frequency=2)
plot.ts(power_ts)

power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)


library(forecast)
arimaModel=auto.arima(power_ts)
powerforecast=forecast(arimaModel,h=3)
accuracy(powerforecast)
plot(powerforecast)
arimaModel



weekday = daysData(energydataset1_renew,"WEEKDAY")


library(xts)
data <- as.xts(weekday$Eact,order.by=as.Date(weekday$event_time))
weekdayxts <- apply.daily(data,sum)
weekday=as.data.frame(index(weekdayxts))
weekdayxts=as.data.frame(weekdayxts)
weekday$weekday_day_power_usage=weekdayxts[1:nrow(weekdayxts),]
colnames(weekday)=c("weekday_dates","weekday_day_power_usage")
rm(weekdayxts)
weekday$daynames=format(weekday$weekday_dates,"%a")


weekday=weekday[weekday$daynames!="Sun", ]

weekday$weekday_dates= strptime(weekday$weekday_dates,"%Y-%m-%d")
weekday$weekday_dates=as.POSIXct(weekday$weekday_dates)





power_ts=ts(weekday$weekday_day_power_usage,start=min(weekday$weekday_dates),frequency=5)
plot.ts(power_ts)

power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)


library(forecast)
arimaModel=auto.arima(power_ts)
powerforecast=forecast(arimaModel,h=3)
accuracy(powerforecast)
plot(powerforecast)
arimaModel




daily$daily_dates= strptime(daily$daily_dates,"%Y-%m-%d")
daily$daily_dates=as.POSIXct(daily$daily_dates)

power_ts=ts(daily$daily_power_usage,start=min(daily$daily_dates),frequency=7)
plot.ts(power_ts)
power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)
library(forecast)
arimaModel=auto.arima(power_ts)
powerforecast=forecast(arimaModel,h=3)
accuracy(powerforecast)





g <- ggplot(energydataset1_renew, aes(energydataset1_renew$event_time))
g <- g + geom_line(aes(y=Eact), colour=as.Date(energydataset1_renew$event_time))
g + labs(x = "TIME",y="Power",title = "power plot")

power_ts=ts(energydataset1_renew$Eact,start=min(energydataset1_renew$event_time),frequency=7*24*60)
plot.ts(power_ts)
power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)
library(forecast)
arimaModel=auto.arima(power_ts)
powerforecast=forecast(arimaModel,h=3)
accuracy(powerforecast)


night<-g+ xlim(as.POSIXct(c('2019-01-12 00:00:44', '2019-01-12 06:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="Power",title = "NIGHT")
morning<-g+ xlim(as.POSIXct(c('2019-01-12 06:00:00', '2019-01-12 10:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="Power",title = "MORNING")
midmorning<- g+ xlim(as.POSIXct(c('2019-01-12 10:00:00', '2019-01-12 15:02:18'), format="%Y-%m-%d %H:%M:%OS"))+ labs(x = "TIME",y="Power",title = "MIDMORNING")


##time divisions in one graph
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))

print(night, vp = vplayout(1,1))
print(morning, vp = vplayout(1,2))
print(midmorning, vp = vplayout(1,3))