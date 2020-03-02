#removing anomalies
energydataset1_renew$Eact[which(energydataset1_renew$switch ==0 & energydataset1_renew$Eact > 0)] = 0
library(xts)
data <- as.xts(energydataset1_renew$Eact,order.by=as.Date(energydataset1_renew$event_time))
dailyxts <- apply.daily(data,sum)
daily=as.data.frame(index(dailyxts))
dailyxts=as.data.frame(dailyxts)
daily$dailyly_power_usage=dailyxts[1:nrow(dailyxts),]
colnames(daily)=c("daily_dates","daily_power_usage")
rm(dailyxts)
daily$daily_dates= strptime(daily$daily_dates,"%Y-%m-%d")

daily$daily_dates=as.POSIXct(daily$daily_dates)

daily$daily_dates= as.POSIXct(daily$daily_dates,format='%Y-%m-%d',tz= "UTC")

power_ts=ts(daily$daily_power_usage,start=min(daily$daily_dates),frequency=7)
plot.ts(power_ts)


power_ts_decomp=decompose(power_ts)#to get random,observed ,seasonal data
plot(power_ts_decomp)
#power_ts = ma(power_ts,7)
power_ts_hw=HoltWinters(power_ts,gamma=F)#Holtwinters 
plot(power_ts_hw)
library(forecast)
#forecast

power_ts_hw_fcst=forecast:::forecast.HoltWinters(power_ts_hw,n.ahead=3)
forecast:::plot.forecast(power_ts_hw_fcst)

plot.ts(power_ts_hw_fcst$residuals)

#different adf testing methods
tseries::adf.test(energydataset1_renew$event_time,k=0)
fUnitRoots::adfTest(energydataset1_renew$date, lags = 10, type = "ct")
CADFtest(energydataset1_renew$date, max.lag.y = 10, type = "trend")
